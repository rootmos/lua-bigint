import argparse
import os

import semver
import github

whoami = "release"
def env(var, default=None):
    return os.environ.get("RELEASE_" + var, default)

import logging
logger = logging.getLogger(whoami)

def parse_args():
    parser = argparse.ArgumentParser(
            description = "release tool",
            formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    parser.add_argument("--log", default=env("LOG_LEVEL", "WARN"), help="set log level")

    return parser.parse_args()

def setup_logger(level):
    logger.setLevel(level)

    ch = logging.StreamHandler()
    ch.setLevel(level)

    f = logging.Formatter(fmt="%(asctime)s:%(name)s:%(levelname)s %(message)s", datefmt="%Y-%m-%dT%H:%M:%S%z")
    ch.setFormatter(f)

    logger.addHandler(ch)

def sandbox(args):
    g = github.Github(auth=github.Auth.Token(os.environ["GITHUB_TOKEN"]))

    repo = g.get_repo("rootmos/h")

    head = repo.get_commit("0a49174efe7d555e4d8a93581958aeb80e9db80b")
    print(head.commit.message)

    # releases = {}
    for r in repo.get_releases():
        print(r.name)
        print(r.tag_name)

def main():
    args = parse_args()
    setup_logger(args.log.upper())
    logger.debug(f"args: {args}")

    sandbox(args)

if __name__ == "__main__":
    main()
