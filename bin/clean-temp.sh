#!/usr/bin/env bash

# Clean abandoned Emacs autosave files
find . | egrep [~#]$ | xargs rm

