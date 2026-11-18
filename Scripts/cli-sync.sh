#!/bin/bash

# PREREQUISITES
#
# The Globus CLI must be installed and have an active login session.
#

# DESCRIPTION
#
# Sync one folder with another, across two endpoints.
# The sync will recurse through all subdirectories.
#
# Source: Globus Tutorial Endpoint 1: /share/godata
# Destination: Globus Tutorial Endpoint 2: /~/sync-demo/ # Your account home directory
#
# Default values are below in CAPS for
# SOURCE_ENDPOINT, DESTINATION_ENDPOINT, SOURCE_PATH,
# DESTINATION_PATH, LAST_TRANSFER_ID_FILE, and SYNCTYPE
#
# Changes these to make this script suit your needs.
#
# Visit https://www.globus.org/app/transfer?destination_id=ddb59af0-6d04-11e5-ba46-22000b92c6ec
# to view the transferred data.

#case="EXP_mes_climatology_1950_1970"
#case="EXP_mes_climatology_1850_1870"
#case="EXP_mes_LSM_new_radiation"
#year="1863"
cfg=$1
case=$2
year=$3
month=$4

# ARCHER2 Endpoint 
SOURCE_ENDPOINT='3e90d018-0d05-461a-bbaf-aab605283d21'

# JASMIN Endpoint 
DESTINATION_ENDPOINT='a2f53b7f-1b4e-4dce-9b7c-349ae760fee0'

# Sample data
SOURCE_PATH="/work/n02/n02/ryapat30/nemo/nemo_4.2.3/cfgs/${cfg}/${case}/${year}/${month}/"
echo $SOURCE_PATH

# Destination Path
# The directory will be created if it doesn't exist
DESTINATION_PATH="/gws/ssde/j25a/verify_oce/NEMO/Outputs/Historical/${case}/${year}/${month}/"
echo $DESTINATION_PATH

# Where the ID of the previous transfer (if exists) is stored
LAST_TRANSFER_ID_FILE='last-transfer-id.txt'

# Sync options:
#   exists   Copy files that do not exist at the destination.
#   size     Copy files if the size of the destination does not match the size of the source.
#   mtime    Copy files if the timestamp of the destination is older than the timestamp of the source.
#   checksum Copy files if checksums of the source and destination do not match. Files on the destination are never deleted.
# For more information:
# $ globus transfer --help
# < OR >
# https://docs.globus.org/api/transfer/task_submit/#transfer_and_delete_documents
SYNCTYPE='checksum'

# always start unset
unset abort_message

# start with default of 0
rc=0

# Abort with message
function abort () {
    echo "$abort_message" >&2
    exit $rc
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
    #if [ $rc -eq 0 ];
    #then
    #    ls "${SOURCE_PATH}/{01..11}"
    #fi
}

# Check if abort is necessary, fetching rc first
function check_last_rc () {
    # must be the first command of the function
    rc=$?
    check_rc "$@"
}

# Only continue if the previous transfer succeeded or failed
# Other statuses will mean that previous transfer is either still
# running or requires human intervention (e.g., PAUSED)

#echo "Checking for a previous transfer"
#
#if [ -e "$LAST_TRANSFER_ID_FILE" ]
#then
#    last_transfer_id=$(cat "$LAST_TRANSFER_ID_FILE")
#    last_transfer_status=$(globus task show --format unix --jmespath 'status' $last_transfer_id)
#    if [ "$last_transfer_status" != "SUCCEEDED" ] && [ "$last_transfer_status" != "FAILED" ]
#       then
#           abort_message="Last transfer $last_transfer_id status is $last_transfer_status, aborting"
#           rc=1
#           abort
#    else
#        echo "Last transfer $last_transfer_id $last_transfer_status, continuing"
#    fi
#fi

# Verify that the source paths is a directory
echo "$SOURCE_ENDPOINT:$SOURCE_PATH"
globus ls --format unix --jmespath 'code' "$SOURCE_ENDPOINT:$SOURCE_PATH"
echo "done"
#globus ls --format unix --jmespath 'code' "$SOURCE_ENDPOINT:$SOURCE_PATH" >& /dev/null
check_last_rc "Could not list source directory" "Verified that source is a directory\n"
echo "done done"

# Submit sync transfer, get the task ID
echo "$SOURCE_ENDPOINT:$SOURCE_PATH"
globus_output=$(globus transfer --format unix --jmespath 'task_id'  --recursive \
                       --delete-destination-extra --sync-level $SYNCTYPE \
                       "$SOURCE_ENDPOINT:$SOURCE_PATH" \
                       "$DESTINATION_ENDPOINT:$DESTINATION_PATH")
echo "done done done"

success_msg="Submitted sync from $SOURCE_ENDPOINT:$SOURCE_PATH to $DESTINATION_ENDPOINT:$DESTINATION_PATH"
source_path_enc=$(echo $SOURCE_PATH | sed 's?/?%%2F?g')
destination_path_enc=$(echo $DESTINATION_PATH | sed 's?/?%%2F?g')
# Note the double percent signs and \n for the printf statement
link="Link:\nhttps://app.globus.org/activity/$globus_output/overview\n"

# Check status
check_last_rc "Globus transfer submission failed" "$success_msg\n$link"

# Save ID of new sync transfer
echo "Saving sync transfer ID to $LAST_TRANSFER_ID_FILE"
echo $globus_output > "$LAST_TRANSFER_ID_FILE"
