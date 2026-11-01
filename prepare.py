#!/usr/bin/env -S uv run --script
import os
import shutil


def main():
    my_env = os.environ.copy()
    if "ELM_HOME" in my_env:
        env_path = my_env["ELM_HOME"]
    elif "APPDATA" in my_env:
        env_path = os.path.join(my_env["APPDATA"], "elm")
    else:
        return
    source_folder = os.path.join(env_path, "0.19.1", "packages")
    cache_folder = "elm-stuff/libraries"
    shutil.copytree(source_folder, cache_folder)
    print(os.listdir(source_folder))
    print(os.listdir(cache_folder))

if __name__ == '__main__':
    main()
