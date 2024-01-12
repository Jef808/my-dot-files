#!/usr/bin/env sh

#            Local Address        Foreign Address        State
# one donnection

ssh -L 8080:localhost:8080 jfa@192.168.1.67
#ssh -L 4045:localhost:4045 jfa@192.168.1.67
