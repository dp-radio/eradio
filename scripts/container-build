#!/usr/bin/env python3

# Copyright (c) 2020 jessa0
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import argparse, configparser, contextlib, http, http.server, json, os, re, shlex, shutil, socket, socketserver
import subprocess, sys, tempfile, threading
from pathlib import Path, PurePath
from subprocess import CalledProcessError
from urllib.parse import urlparse

CONFIG_DIRECTORY = 'container-build'

DEFAULT_APT_KEYS            = str(Path(CONFIG_DIRECTORY, 'apt-keys'))
DEFAULT_APT_CONF_FILE       = str(Path(CONFIG_DIRECTORY, 'apt.conf'))
DEFAULT_APT_SOURCES_FILE    = str(Path(CONFIG_DIRECTORY, 'sources.list'))
DEFAULT_BASE_IMAGE          = 'debian:stable-slim'
DEFAULT_CONFIG_FILE         = str(Path(CONFIG_DIRECTORY, 'build.cfg'))
DEFAULT_DOCKER              = 'docker'
DEFAULT_DOCKER_HOST         = 'unix:///var/run/docker.sock'
DEFAULT_HOME_DIR_PREFIX     = '/home/'
DEFAULT_INSTALL_SCRIPT      = str(Path(CONFIG_DIRECTORY, 'install.sh'))
DEFAULT_PACKAGES_FILE       = str(Path(CONFIG_DIRECTORY, 'packages'))
DEFAULT_SHELL               = '/bin/bash'
DEFAULT_USERNAME            = 'build'
DEFAULT_USER_INSTALL_SCRIPT = str(Path(CONFIG_DIRECTORY, 'user_install.sh'))
DEFAULT_WORK_DIR            = 'src'

SCRIPTS_DIR = 'scripts'
APT_KEYS_DIR = 'apt-keys'


def main():
    args = arg_parser().parse_args()
    try:
        config = ConfigMerger(args)
    except ConfigSectionMissing as ex:
        print(f'Specified section missing in config file: \'{ex}\'')

    opts = Options(config)

    if opts.verbose >= 1 and config.config_file is not None:
        print(f'Read config file {config.config_file}', file=sys.stderr)

    try:
        packages = read_packages(opts.packages_file)
    except OSError as ex:
        print(f'Error opening packages file \'{opts.packages_file}\': {ex}', file=sys.stderr)
        exit(1)

    packages.extend(opts.package)
    packages = sorted(packages)

    apt_sources_src = opts.apt_sources_file
    if opts.apt_sources_file is not None:
        apt_sources = Path(opts.apt_sources_file).name
    else:
        apt_sources = None

    apt_conf_src = opts.apt_conf_file
    if opts.apt_conf_file is not None:
        apt_conf = Path(opts.apt_conf_file).name
    else:
        apt_conf = None

    if opts.apt_keys is not None:
        apt_keys_src = []
        apt_keys = []
        for apt_key_name in sorted(os.listdir(opts.apt_keys)):
            apt_keys_src.append(Path(opts.apt_keys, apt_key_name))
            apt_keys.append(Path(APT_KEYS_DIR, apt_key_name))
    else:
        apt_keys_src = None
        apt_keys = None

    install_scripts_src = opts.install_script
    install_scripts = []
    for (install_script_index, install_script_src) in enumerate(install_scripts_src or []):
        install_script_name = Path(install_script_src).name
        install_scripts.append(Path(SCRIPTS_DIR, f'{install_script_index}_{install_script_name}'))

    user_install_scripts_src = opts.user_install_script
    user_install_scripts = []
    for (user_install_script_index, user_install_script_src) in enumerate(user_install_scripts_src or []):
        user_install_script_name = Path(user_install_script_src).name
        user_install_scripts.append(Path(SCRIPTS_DIR, f'{user_install_script_index}_{user_install_script_name}'))

    work_dir = str(Path(opts.home_dir, opts.work_dir))

    groups = []

    if opts.uid == 0 or opts.gid == 0:
        print('Cannot run command as root in container (use the --uid and --gid arguments).', file=sys.stderr)
        exit(1)

    try:
        volumes = collect_volumes(opts.mount, work_dir, not opts.no_recursive_mount)
    except FileNotFoundError as ex:
        print(f'Error resolving mount path: {ex}', file=sys.stderr)
        exit(1)

    if opts.mount_home_dir is not None and opts.mount_home_dir is not True:
        home_dir = Path(opts.mount_home_dir).resolve()
        volumes[str(home_dir)] = opts.home_dir

    if opts.docker_passthrough or opts.docker_proxy:
        docker_host = urlparse(opts.docker_host)
        if docker_host.scheme != 'unix':
            print(f'Passthrough of daemon socket scheme \'{docker_host.scheme}\' not supported', file=sys.stderr)
            exit(1)

    if opts.docker_passthrough:
        try:
            passthrough_sock_dst = Path(docker_host.path)
            passthrough_sock_src = passthrough_sock_dst.resolve()
            volumes[str(passthrough_sock_src)] = str(passthrough_sock_dst)

            passthrough_sock_stat = passthrough_sock_src.stat()
            if passthrough_sock_stat.st_uid != opts.uid:
                if passthrough_sock_stat.st_mode & 0o060 != 0o060:
                    print(f'Passthrough of daemon socket \'{docker_host}\' not writable by group owner unsupported',
                          file=sys.stderr)
                    exit(1)
                if passthrough_sock_stat.st_gid == 0:
                    print(f'Passthrough of daemon socket \'{docker_host}\' owned by group 0 not supported',
                          file=sys.stderr)
                    exit(1)
                groups.append(str(passthrough_sock_stat.st_gid))
        except (OSError, KeyError) as ex:
            print(f'Error fetching passthrough socket path: {ex}', file=sys.stderr)
            exit(1)

    dockerfile_data = generate_dockerfile(
        base_image=opts.base_image,
        username=opts.username,
        home_dir=opts.home_dir,
        shell=opts.shell,
        work_dir=work_dir,
        env=opts.env,
        apt_conf=apt_conf,
        apt_sources=apt_sources,
        apt_keys=apt_keys,
        packages=packages,
        install_scripts=install_scripts,
        user_install_scripts=user_install_scripts,
    )

    with create_dirs(opts.directory, opts.docker_proxy, opts.mount_home_dir is True) as dirs:
        (build_dir, docker_proxy_dir, home_dir) = dirs
        dockerfile_path = Path(build_dir, 'Dockerfile')

        try:
            with open(dockerfile_path, mode='w', encoding='utf-8') as dockerfile:
                dockerfile.write(dockerfile_data)
        except OSError as ex:
            print(f'error writing Dockerfile at {dockerfile_path}: {ex}', file=sys.stderr)
            exit(1)

        if opts.verbose >= 1:
            print(f'wrote Dockerfile at {dockerfile_path}:\n{dockerfile_data}', file=sys.stderr)

        build_src_dsts = []
        if apt_sources_src is not None:
            build_src_dsts.append((apt_sources_src, apt_sources))
        if apt_conf_src is not None:
            build_src_dsts.append((apt_conf_src, apt_conf))
        if apt_keys_src is not None:
            build_src_dsts.extend(zip(apt_keys_src, apt_keys))
        if install_scripts_src is not None:
            build_src_dsts.extend(zip(install_scripts_src, install_scripts))
        if user_install_scripts_src is not None:
            build_src_dsts.extend(zip(user_install_scripts_src, user_install_scripts))
        if not copy_build_files(build_src_dsts, build_dir, opts.verbose):
            exit(1)

        if opts.docker_proxy:
            docker_proxy_path = Path(docker_proxy_dir, 'docker.sock')
            volumes[str(docker_proxy_path)] = str(docker_host.path)

        if home_dir:
            volumes[home_dir] = opts.home_dir

        container = create_docker_container(
            docker=opts.docker,
            docker_create_flags=opts.docker_create_flags,
            image_name=opts.image_name,
            build_dir=build_dir,
            dockerfile_path=dockerfile_path,
            uid=opts.uid,
            gid=opts.gid,
            groups=groups,
            tty=not opts.no_tty,
            volumes=volumes.items(),
            command=opts.command,
            verbose=opts.verbose,
        )
        if container is None:
            exit(1)

        if opts.docker_proxy:
            docker_proxy = DockerProxy(str(docker_proxy_path), str(docker_host.path), volumes.items(),
                                       container.rootfs, opts.verbose)
            docker_proxy.start()

        if not container.start(docker_start_flags=opts.docker_start_flags):
            exit(1)


def infer_name():
    dir_name = Path.cwd().name
    return f'{dir_name}-builder'


def read_packages(packages_path):
    with open(packages_path, mode='r', encoding='utf-8') as packages_file:
        packages = []
        for package in re.split(r'\s+', packages_file.read()):
            if package:
                packages.append(package)
        return packages


def collect_volumes(mount_args, work_dir, recursive):
    volumes = {}
    for mount_arg in mount_args:
        mount_src_str, *mount_dst_list = mount_arg.split(':', 1)
        mount_src_path = Path(mount_src_str)
        mount_src = mount_src_path.resolve()
        if len(mount_dst_list) == 1:
            mount_dst = Path(work_dir, mount_dst_list[0])
        else:
            mount_dst = Path(work_dir, mount_src_path.name)
        volumes[str(mount_src)] = str(mount_dst)
        if recursive and mount_src.is_dir():
            with os.scandir(mount_src) as subdirs:
                for subdir in subdirs:
                    if subdir.is_symlink() and subdir.is_dir():
                        subdir_src = Path(subdir.path).resolve()
                        subdir_dst = Path(mount_dst, subdir.name)
                        volumes[str(subdir_src)] = str(subdir_dst)
    return volumes


@contextlib.contextmanager
def create_dirs(directory, docker_proxy, mount_home_dir):
    if directory is not None:
        os.makedirs(directory, mode=0o755, exist_ok=True)
        if not docker_proxy and not mount_home_dir:
            yield directory, None, None
            return

    with tempfile.TemporaryDirectory() as temp_dir:
        if directory is None:
            directory = Path(temp_dir, "build")
            os.mkdir(directory, mode=0o755)
        docker_proxy_dir = None
        if docker_proxy:
            docker_proxy_dir = Path(temp_dir, "docker_proxy")
            os.mkdir(docker_proxy_dir, mode=0o750)
        home_dir = None
        if mount_home_dir:
            home_dir = Path(temp_dir, "home")
            os.mkdir(home_dir, mode=0o755)
        yield directory, docker_proxy_dir, home_dir


def arg_parser():
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description='A tool to run a command within a generated container, geared toward build systems.',
        epilog=f'''\
environment variables:
  DOCKER                Path to docker executable. Defaults to '{DEFAULT_DOCKER}'.
  DOCKER_CREATE_FLAGS   Extra flags to pass to 'docker create' command.
  DOCKER_START_FLAGS    Extra flags to pass to 'docker start' command.
  DOCKER_HOST           Docker daemon socket to connect to. Defaults to '{DEFAULT_DOCKER_HOST}'.

The config file is in ini-style format and can contain any long-form command line argument or environment variable. Only
the first section in the config file is used, and the name of the generated container will default to the name of that
section.'''
    )
    parser.add_argument('command', nargs='*',
                        help='Command to run within the container.')
    parser.add_argument('-c', '--config-file',
                        help=f'Path of config file. Defaults to \'{DEFAULT_CONFIG_FILE}\', if it exists.')
    parser.add_argument('--no-config-file',
                        help=f'Suppress using default config file path \'{DEFAULT_CONFIG_FILE}\'.')
    parser.add_argument('-s', '--config-section',
                        help='Section of config file to use. Defaults to the first section in the config file.')
    parser.add_argument('-n', '--name',
                        help='Name of generated container image. Defaults to the name of the current working'
                        ' directory suffixed with \'-builder\'.')
    parser.add_argument('-d', '--directory',
                        help='Path to directory to write generated files. Defaults to using a temporary directory.')
    parser.add_argument('--install-script', action='append',
                        help='Path of extra script to run as root in container during image creation. May be specified'
                        f' multiple times. Defaults to \'{DEFAULT_INSTALL_SCRIPT}\', if it exists.')
    parser.add_argument('--no-install-script', action='store_const', const=True,
                        help=f'Suppress using the default install script path \'{DEFAULT_INSTALL_SCRIPT}\'.')
    parser.add_argument('--user-install-script', action='append',
                        help='Path of extra script to run as build user in container during image creation. May be specified'
                        f' multiple times. Defaults to \'{DEFAULT_USER_INSTALL_SCRIPT}\', if it exists.')
    parser.add_argument('--no-user-install-script', action='store_const', const=True,
                        help=f'Suppress using the default user install script path \'{DEFAULT_USER_INSTALL_SCRIPT}\'.')
    parser.add_argument('--base-image',
                        help=f'Base image to derive the container from. Defaults to \'{DEFAULT_BASE_IMAGE}\'.')
    parser.add_argument('-p', '--package', action='append',
                        help='Apt package specification of package to install in the container. May be specified'
                        ' multiple times.')
    parser.add_argument('--packages-file',
                        help='Path of file containing apt package specifications to install in the container. Defaults'
                        f' to \'{DEFAULT_PACKAGES_FILE}\'.')
    parser.add_argument('--apt-conf-file',
                        help='Path of apt.conf to use during package installation in the container. Defaults to'
                        f' \'{DEFAULT_APT_CONF_FILE}\', if it exists.')
    parser.add_argument('--no-apt-conf-file', action='store_const', const=True,
                        help=f'Suppress using the default apt.conf path \'{DEFAULT_APT_CONF_FILE}\'.')
    parser.add_argument('--apt-sources-file',
                        help='Path of apt sources.list to use during package installation in the container. Defaults to'
                        f' \'{DEFAULT_APT_SOURCES_FILE}\', if it exists.')
    parser.add_argument('--no-apt-sources-file', action='store_const', const=True,
                        help=f'Suppress using the default apt sources path \'{DEFAULT_APT_SOURCES_FILE}\'.')
    parser.add_argument('--apt-keys',
                        help='Path of directory containing .gpg files to install using apt-key in the container.'
                        f' Defaults to \'{DEFAULT_APT_KEYS}\', if it exists.')
    parser.add_argument('--no-apt-keys', action='store_const', const=True,
                        help=f'Suppress using the default apt keys path \'{DEFAULT_APT_KEYS}\'.')
    parser.add_argument('-u', '--uid', type=int,
                        help='UID used to run COMMAND in the container. Defaults to current euid.')
    parser.add_argument('-g', '--gid', type=int,
                        help='GID used to run COMMAND in the container. Defaults to current egid.')
    parser.add_argument('--username',
                        help=f'Username used to run COMMAND in the container. Defaults to \'{DEFAULT_USERNAME}\'.')
    parser.add_argument('--home-dir',
                        help='Path of home directory used in the container. Defaults to'
                        f' \'{DEFAULT_HOME_DIR_PREFIX}$USERNAME\'.')
    parser.add_argument('--shell',
                        help=f'Path of shell used to run COMMAND in the container. Defaults to \'{DEFAULT_SHELL}\'.')
    parser.add_argument('--work-dir',
                        help='Path of working directory to run COMMAND in the container, optionally relative to the'
                        f' home directory. Defaults to \'{DEFAULT_WORK_DIR}\'.')
    parser.add_argument('--env', action='append', metavar='NAME=VALUE',
                        help='Sets a variable in the environment used to run COMMAND in the container. May be specified'
                        f' multiple times.')
    parser.add_argument('--no-tty', action='store_const', const=True,
                        help='Suppress allocating a tty to run COMMAND in the container.')
    parser.add_argument('--mount-home-dir', help='Directory to bind mount as the home directory used in the container.'
                        ' Defaults to a temporary directory.')
    parser.add_argument('--no-mount-home-dir', action='store_const', const=True,
                        help='Suppress mounting a temporary directory as the home directory used in the container.')
    parser.add_argument('-m', '--mount', action='append',
                        help='Directory to bind mount under the working directory in the container. Mount location may'
                        ' be specified followed by a colon, like \'src:dst\'. May be specified multiple times. Defaults'
                        ' to the current directory.')
    parser.add_argument('--no-recursive-mount', action='store_const', const=True,
                        help='Suppress recursively mounting symlinks to directories outside their containing mount.')
    parser.add_argument('--docker-passthrough', action='store_const', const=True,
                        help='Mount docker unix socket from host inside container, and add user to group owning the'
                        ' socket inside the container.')
    parser.add_argument('--docker-proxy', action='store_const', const=True,
                        help='Make docker unix socket from host available inside container using a unix socket proxy.'
                        ' Rewrites volume paths to refer to the container\'s filesystem.')
    parser.add_argument('-v', '--verbose', action='count',
                        help='Enable verbose output. May be specified multiple times for more verbosity.')
    return parser


def generate_dockerfile(base_image, username, home_dir, shell, work_dir, env, apt_conf, apt_sources, apt_keys, packages,
                        install_scripts, user_install_scripts):
    pre_packages = []
    if apt_sources:
        pre_packages.append('apt-transport-https')
    if apt_keys:
        pre_packages.extend(['gnupg', 'software-properties-common'])

    apt_keys_dst = []
    for apt_key in apt_keys or []:
        apt_keys_dst.append(f'/tmp/build/{apt_key}')

    dockerfile = f'''\
FROM {base_image}

ARG UID
ARG GID

'''

    if apt_keys:
        dockerfile += f'''\
COPY [ "{APT_KEYS_DIR}", "/tmp/build/{APT_KEYS_DIR}" ]

'''

    if pre_packages:
        pre_package_args = ' \\\n       '.join(pre_packages)
        dockerfile += f'''\
RUN    for retry in {{0..10}}; do apt-get update && break; done \\
    && apt-get install --no-install-recommends -y -o APT::Acquire::Retries=10 \\
       {pre_package_args} \\
'''
    if apt_keys:
        assert pre_packages
        apt_key_args = ' '.join(apt_keys_dst)
        dockerfile += f'''\
    && apt-key add {apt_key_args} \\
    && rm -rf /tmp/build \\
'''

    if pre_packages:
        dockerfile += '''\
    && rm -rf /var/lib/apt/lists/*

'''

    if apt_conf:
        apt_conf_path = Path(apt_conf)
        dockerfile += f'''\
COPY [ "{apt_conf_path}", "/etc/apt/apt.conf.d/99container-build.conf" ]

'''
    if apt_sources:
        apt_sources_path = Path(apt_sources)
        dockerfile += f'''\
COPY [ "{apt_sources_path}", "/etc/apt/sources.list.d/container-build.list" ]

'''

    if packages:
        package_args = ' \\\n       '.join(packages)
        dockerfile += f'''\
RUN    for retry in {{0..10}}; do apt-get update && break; done \\
    && apt-get install --no-install-recommends -y -o APT::Acquire::Retries=10 \\
       {package_args} \\
    && rm -rf /var/lib/apt/lists/*

'''

    for install_script_str in install_scripts or []:
        install_script = Path(install_script_str)
        dockerfile += f'''\
COPY [ "{install_script}", "/tmp/build/{install_script.name}" ]
RUN    '/tmp/build/{install_script.name}' \\
    && rm -rf /tmp/build

'''

    dockerfile += f'''\
# Create a user to map the host user to.
RUN    groupadd -o -g ${{GID}} '{username}' \\
    && useradd -m -o -u ${{UID}} -g ${{GID}} -s '{shell}' -d '{home_dir}' '{username}'
USER {username}
ENV HOME {home_dir}
ENV USER {username}
ENV LC_ALL C.UTF-8
WORKDIR {work_dir}

'''

    for user_install_script_str in user_install_scripts or []:
        user_install_script = Path(user_install_script_str)
        dockerfile += f'''\
COPY --chown=${{UID}}:${{GID}} [ "{user_install_script}", "/tmp/build/{user_install_script.name}" ]
RUN    '/tmp/build/{user_install_script.name}' \\
    && rm -rf /tmp/build

'''

    for env_var in env:
        env_var_name, env_var_value = env_var.split('=', 1)
        if env_var_value:
            dockerfile += f'''\
ENV {env_var_name} {env_var_value}
'''
        else:
            dockerfile += f'''\
ENV {env_var_name}=""
'''

    dockerfile += f'''\
CMD [ "{shell}" ]
'''
    return dockerfile


def copy_build_files(src_dsts, build_dir, verbose):
    for (src, dst) in src_dsts:
        full_dst = Path(build_dir, dst)
        try:
            if verbose >= 2:
                print(f'copying file \'{src}\' to build context \'{full_dst}\'', file=sys.stderr)
            os.makedirs(full_dst.parent, mode=0o755, exist_ok=True)
            shutil.copy2(src, str(full_dst))
        except OSError as ex:
            print(f'error copying file \'{src}\' to build context: {ex}', file=sys.stderr)
            return False
    return True


def create_docker_container(docker, docker_create_flags, image_name, build_dir, dockerfile_path, uid, gid, groups,
                            tty, volumes, command, verbose):
    try:
        docker_build_subprocess_args = {}
        docker_build_args = [
            docker, 'build',
            '--build-arg', f'UID={uid}',
            '--build-arg', f'GID={gid}',
            '--tag', image_name,
            '--file', str(dockerfile_path),
        ]

        if verbose < 1:
            docker_build_args.append('--quiet')
            docker_build_subprocess_args['stdout'] = subprocess.DEVNULL

        docker_build_args.append(str(build_dir))

        if verbose >= 1:
            print('running ' + ' '.join(docker_build_args), file=sys.stderr)

        subprocess.run(docker_build_args, check=True, **docker_build_subprocess_args)
    except CalledProcessError as ex:
        print(f'docker build returned {ex.returncode}', file=sys.stderr)
        return None

    try:
        docker_create_args = [docker, 'create', '--interactive', '--rm']
        if tty:
            docker_create_args.append('--tty')
        if len(groups) != 0:
            docker_create_args.extend(['--group-add', ','.join(groups)])
        docker_create_args.extend(shlex.split(docker_create_flags))
        for (host_dir, container_dir) in volumes:
            docker_create_args.extend(['--volume', f'{host_dir}:{container_dir}'])
        docker_create_args.append(image_name)
        docker_create_args.extend(command)

        if verbose >= 1:
            print('running ' + ' '.join(docker_create_args), file=sys.stderr)

        docker_create_proc = subprocess.run(docker_create_args, check=True, stdout=subprocess.PIPE)
        container_id = str(docker_create_proc.stdout.strip(), 'ascii')
    except CalledProcessError as ex:
        print(f'docker create returned {ex.returncode}', file=sys.stderr)
        return None

    return DockerContainer(container_id, docker, verbose)


class ConfigSectionMissing(Exception):
    pass


class ConfigMerger:
    def __init__(self, args):
        self.args = args
        self.config = None
        self.config_section = None

        self.config_file = self.get_file('config-file', DEFAULT_CONFIG_FILE)
        if self.config_file is not None:
            config = configparser.ConfigParser(allow_no_value=True, strict=True)
            config.read(self.config_file)
            if args.config_section is not None:
                if args.config_section not in config.sections():
                    raise ConfigSectionMissing(args.config_section)
                self.config = config
                self.config_section = args.config_section
            elif len(config.sections()) != 0:
                self.config = config
                self.config_section = config.sections()[0]

    def get(self, name, default=None):
        return self.get_or_else(name, lambda: default)

    def get_or_else(self, name, default=None):
        arg = getattr(self.args, name.replace('-', '_'), None)
        if arg is not None:
            return arg
        if self.config is not None:
            if self.config.has_option(self.config_section, name):
                config_value = self.config.get(self.config_section, name)
                if config_value is not None:
                    return config_value
                return True
        if default:
            return default()

    def get_flag(self, name):
        value = self.get(name)
        if isinstance(value, bool):
            return value
        return value is not None

    def get_list(self, name, default=[], delimiter="\n"):
        unsplit_values = []

        if self.config is not None:
            if self.config.has_option(self.config_section, name):
                config_value = self.config.get(self.config_section, name)
                if config_value is not None:
                    unsplit_values.append(config_value)

        arg = getattr(self.args, name.replace('-', '_'), None)
        if isinstance(arg, list):
            unsplit_values.extend(arg)
        elif arg is not None:
            unsplit_values.append(arg)

        if len(unsplit_values) == 0:
            return default

        values = []
        for unsplit_value in unsplit_values:
            for value in re.split(delimiter, unsplit_value):
                if value:
                    values.append(value)
        return values

    def get_env(self, name, default=None):
        env = os.getenv(name)
        if env is not None:
            return env
        return self.get(name.lower(), default)

    def get_file(self, name, default):
        arg = self.get(name)
        if arg is not None:
            return arg
        if self.get(f'no-{name}'):
            return None

        if isinstance(default, list):
            present = []
            for default_path in default:
                if os.path.exists(default_path):
                    present.append(default_path)
            if present:
                return present
        elif isinstance(default, str) or isinstance(default, bytes):
            if os.path.exists(default):
                return default
        else:
            return default

    def get_file_list(self, name, default):
        value = self.get_file(name, default)
        if isinstance(value, list):
            return value
        if value is not None:
            return [value]
        return []


class Options:
    def __init__(self, config):
        self.apt_keys            = config.get_file('apt-keys', DEFAULT_APT_KEYS)
        self.apt_conf_file       = config.get_file('apt-conf-file', DEFAULT_APT_CONF_FILE)
        self.apt_sources_file    = config.get_file('apt-sources-file', DEFAULT_APT_SOURCES_FILE)
        self.base_image          = config.get('base-image', DEFAULT_BASE_IMAGE)
        self.command             = config.get('command')
        self.directory           = config.get('directory')
        self.docker              = config.get_env("DOCKER", DEFAULT_DOCKER)
        self.docker_host         = config.get_env('DOCKER_HOST', DEFAULT_DOCKER_HOST)
        self.docker_passthrough  = config.get_flag('docker-passthrough')
        self.docker_proxy        = config.get_flag('docker-proxy')
        self.docker_create_flags = config.get_env('DOCKER_CREATE_FLAGS', '')
        self.docker_start_flags  = config.get_env('DOCKER_START_FLAGS', '')
        self.env                 = config.get_list('env')
        self.gid                 = config.get_or_else('gid', os.getegid)
        self.image_name          = config.get_or_else('name', lambda: config.config_section or infer_name())
        self.home_dir            = config.get('home-dir')
        self.install_script      = config.get_file_list('install-script', [DEFAULT_INSTALL_SCRIPT])
        self.mount               = config.get_list('mount', default=['.'])
        self.mount_home_dir      = config.get_file('mount-home-dir', True)
        self.no_recursive_mount  = config.get_flag('no-recursive-mount')
        self.no_tty              = config.get_flag('no-tty')
        self.package             = config.get_list('package', delimiter=r'\s+')
        self.packages_file       = config.get('packages-file', DEFAULT_PACKAGES_FILE)
        self.uid                 = config.get_or_else('uid', os.geteuid)
        self.username            = config.get('username', DEFAULT_USERNAME)
        self.user_install_script = config.get_file_list('user-install-script', [DEFAULT_USER_INSTALL_SCRIPT])
        self.shell               = config.get('shell', DEFAULT_SHELL)
        self.verbose             = int(config.get('verbose', 0))
        self.work_dir            = config.get('work-dir', DEFAULT_WORK_DIR)

        if self.home_dir is None:
            self.home_dir = DEFAULT_HOME_DIR_PREFIX + self.username


class DockerContainer:
    def __init__(self, container_id, docker, verbose):
        self.container_id = container_id
        self.docker = docker
        self.verbose = verbose

    def rootfs(self):
        try:
            docker_inspect_args = [self.docker, 'inspect', self.container_id]
            if self.verbose >= 1:
                print('running ' + ' '.join(docker_inspect_args), file=sys.stderr)
            docker_inspect_proc = subprocess.run(docker_inspect_args, check=True, stdout=subprocess.PIPE)
            docker_inspect = json.loads(docker_inspect_proc.stdout)
            # NB: 'MergedDir' is specific to the 'overlay' storage driver. Might want to add support for other drivers.
            return docker_inspect[0]['GraphDriver']['Data'].get('MergedDir')
        except CalledProcessError as ex:
            print(f'docker inspect returned {ex.returncode}', file=sys.stderr)
            return None

    def start(self, docker_start_flags):
        try:
            docker_start_args = [self.docker, 'start', '--attach', '--interactive']
            docker_start_args.extend(shlex.split(docker_start_flags))
            docker_start_args.append(self.container_id)
            if self.verbose >= 1:
                print('running ' + ' '.join(docker_start_args), file=sys.stderr)
            subprocess.run(docker_start_args, check=True)
        except CalledProcessError as ex:
            print(f'docker exec returned {ex.returncode}', file=sys.stderr)
            return False
        return True


class DockerProxy:
    def __init__(self, listen_path, target_host, volumes, container_rootfs_func, verbose):
        self.target_host = target_host
        self.server = socketserver.ThreadingUnixStreamServer(listen_path, self.handle_request)
        self.server.daemon_threads = True
        self.container_rootfs_func = container_rootfs_func
        self.volumes = volumes
        self.verbose = verbose

    def start(self):
        self.server_thread = threading.Thread(target=self.server.serve_forever, daemon=True)
        self.server_thread.start()

    def handle_request(self, request, client_address, server):
        return DockerProxyRequestHandler(request, client_address, server, self)


class DockerProxyRequestHandler(http.server.BaseHTTPRequestHandler):
    protocol_version = 'HTTP/1.1'

    def __init__(self, request, client_address, server, docker_proxy):
        self.__target_conn = None
        self.__target_host = docker_proxy.target_host
        self.__container_rootfs_func = docker_proxy.container_rootfs_func
        self.__volumes = docker_proxy.volumes
        self.__verbose = docker_proxy.verbose
        super().__init__(request, client_address, server)

    def log_message(self, fmt, *args):
        pass

    def handle_one_request(self):
        try:
            self.raw_requestline = self.rfile.readline()
            if not self.raw_requestline:
                self.close_connection = True
                return
            if not self.parse_request():
                return
            self.__handle_request()
            self.wfile.flush()
        except (socket.timeout, ConnectionError) as ex:
            if self.__verbose >= 2:
                print(f'error proxying docker request: {ex}', file=sys.stderr)
            self.close_connection = True

    def __handle_request(self):
        if self.__verbose >= 2:
            print(f'proxying docker request: {self.command} {self.path}', file=sys.stderr)
        request_content_iter = self.__stream_request_body()
        request_content = self.__mangle_request(request_content_iter)
        response = self.__proxy_request(request_content)
        self.__write_response(response)

    def __stream_request_body(self):
        request_content_length = self.headers.get('Content-Length')
        if request_content_length is not None:
            return self.__stream_fixed_request_body(int(request_content_length))
        elif self.headers.get('Transfer-Encoding', '').startswith('chunked'):
            return self.__stream_chunked_request_body()
        else:
            return None

    def __stream_fixed_request_body(self, content_left):
        while content_left != 0:
            content_chunk = self.rfile.read1(content_left)
            if len(content_chunk) == 0:
                raise ValueError('EOF before end of request')
            content_left -= len(content_chunk)
            yield content_chunk

    def __stream_chunked_request_body(self):
        while True:
            chunk_line = str(self.rfile.readline(), 'ascii')
            if not chunk_line.endswith('\r\n'):
                raise ValueError('Chunk header did not end in CRLF')
            chunk_line = chunk_line[:-2]

            semicolon_index = chunk_line.find(';')
            if semicolon_index != -1:
                chunk_line = chunk_line[:semicolon_index]

            chunk_length = int(chunk_line, base=16)
            if chunk_length == 0:
                break

            chunk_content_left = chunk_length
            while chunk_content_left != 0:
                chunk_content = self.rfile.read1(chunk_content_left)
                if len(chunk_content) == 0:
                    raise ValueError('EOF before end of chunk')
                chunk_content_left -= len(chunk_content)
                yield chunk_content

            if self.rfile.read(2) != b'\r\n':
                raise ValueError('Chunk data did not end in CRLF')

        while True:
            trailer_line = str(self.rfile.readline(), 'ascii')
            if not trailer_line.endswith('\r\n'):
                raise ValueError('Chunked encoding trailer did not end in CRLF')
            if len(trailer_line) == 2:
                break

    def __mangle_request(self, request_content):
        if self.command == 'POST' and self.path.endswith('/containers/create'):
            request_content = self.__mangle_container_create(request_content)
        else:
            return request_content

        del self.headers['Content-Length']
        del self.headers['Transfer-Encoding']
        self.headers['Content-Length'] = len(request_content)
        return [request_content]

    def __mangle_container_create(self, request_content):
        request_content = b''.join(request_content)
        if self.__verbose >= 2:
            print(f'mangling docker container create request: {request_content}', file=sys.stderr)
        request_json = json.loads(request_content)
        if 'HostConfig' in request_json and 'Binds' in request_json['HostConfig']:
            binds = []
            for bind in request_json['HostConfig']['Binds']:
                bind_src, bind_dst = bind.split(':', 1)
                new_bind_src = None
                for volume_host_dir, volume_container_dir in self.__volumes:
                    try:
                        relative_bind_src = PurePath(bind_src).relative_to(volume_container_dir)
                    except ValueError:
                        relative_bind_src = None

                    if relative_bind_src is not None:
                        new_bind_src = str(PurePath(volume_host_dir, relative_bind_src))
                        break
                if new_bind_src is None:
                    if self.__container_rootfs_func is not None:
                        relative_bind_src = PurePath(bind_src).relative_to('/')
                        new_bind_src = str(PurePath(self.__container_rootfs_func(), relative_bind_src))
                    else:
                        new_bind_src = bind_src
                        if self.__verbose >= 1:
                            print('could not rewrite docker container create request bind as container rootfs path'
                                  f' could not be discovered: {bind_src}', file=sys.stderr)
                if self.__verbose >= 2 and bind_src != new_bind_src:
                    print(f'rewriting docker container create request bind: {bind_src} -> {new_bind_src}',
                          file=sys.stderr)
                binds.append(':'.join([new_bind_src, bind_dst]))
            request_json['HostConfig']['Binds'] = binds
        return json.dumps(request_json, separators=(',', ':')).encode('utf-8')

    def __proxy_request(self, request_content):
        if self.__target_conn is None:
            self.__target_conn = UnixSocketHTTPConnection(self.__target_host)
        connection = self.__target_conn

        connection.putrequest(self.command, self.path, skip_host=True, skip_accept_encoding=True)
        for header_name, header_value in self.headers.items():
            connection.putheader(header_name, str(header_value))

        encode_chunked = self.headers.get('Transfer-Encoding', '').startswith('chunked')
        connection.endheaders(request_content, encode_chunked=encode_chunked)

        return self.__target_conn.getresponse()

    def __write_response(self, response):
        self.send_response_only(response.code)
        for header_name, header_value in response.getheaders():
            self.send_header(header_name, header_value)
        self.end_headers()
        self.wfile.flush()

        if response.chunked:
            if self.__verbose >= 2:
                print(f'proxying docker chunked response: {response.code} {response.getheaders()}',
                      file=sys.stderr)
            self.__proxy_chunked_response_body(response)
        elif response.code == 101:
            if self.__verbose >= 2:
                print(f'proxied docker connection switching protocols: {response.code} {response.getheaders()}',
                      file=sys.stderr)
            response.length = None
            response.will_close = True
            self.__start_proxy_stream(response)
        elif response.length is not None:
            response_content = response.read()
            if self.__verbose >= 2:
                print(f'proxying docker response: {response.code} {response.getheaders()} {response_content}',
                      file=sys.stderr)
            if len(response_content) != 0:
                self.wfile.write(response_content)
                self.wfile.flush()

    def __proxy_chunked_response_body(self, response):
        while True:
            content = response.read1()
            chunk = f'{len(content):X}\r\n'.encode('ascii') + content + b'\r\n'
            self.wfile.write(chunk)
            self.wfile.flush()
            if len(content) == 0:
                break

    def __start_proxy_stream(self, response):
        reader_thread = threading.Thread(target=self.__proxy_request_stream, daemon=True)
        reader_thread.start()
        self.__proxy_response_stream(response)
        reader_thread.join()

    def __proxy_request_stream(self):
        while True:
            data = self.rfile.read1()
            if len(data) == 0:
                break
            self.__target_conn.send(data)

    def __proxy_response_stream(self, response):
        while True:
            data = response.read1()
            if len(data) == 0:
                break
            self.wfile.write(data)
            self.wfile.flush()


class UnixSocketHTTPConnection(http.client.HTTPConnection):
    def __init__(self, path, **kwargs):
        self.__path = path
        super().__init__(path, **kwargs)

    def connect(self):
        self.sock = socket.socket(socket.AF_UNIX)
        self.sock.connect(self.__path)


if __name__ == '__main__':
    main()
