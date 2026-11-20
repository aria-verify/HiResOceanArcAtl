module load globus-cli

rc=0

# Abort with message
function abort () {
    echo "$abort_message" >&2
}

# Check if abort is necessary, and if so do it
function check_rc () {
    if [ $# -gt 0 ];
    then
        abort_message="$1"
    fi

    if [ $rc -ne 0 ];
    then
        abort
    fi

    if [ $rc -eq 0 -a "$2" != "" ];
    then
        printf "$2"
    fi
}

# Check if abort is necessary, fetching rc first
function check_last_rc () {
    # must be the first command of the function
    rc=$?
    check_rc "$@"
}

globus ls --format UNIX --jmespath 'code' "3e90d018-0d05-461a-bbaf-aab605283d21:/work/n02/n02/ryapat30/nemo/nemo_4.2.3/cfgs/NAARC/EXP_mes_climatology_1850_1870/1854/"  >& /dev/null
#globus ls --format UNIX --filter 1854 --jmespath 'DATA[0].type ' "3e90d018-0d05-461a-bbaf-aab605283d21:/work/n02/n02/ryapat30/nemo/nemo_4.2.3/cfgs/NAARC/EXP_mes_climatology_1850_1870/"
check_last_rc "Could not list source directory" "Verified that source is a directory\n"

