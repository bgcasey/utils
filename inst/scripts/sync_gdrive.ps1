# Sync local directory to Google Drive using rclone

# Set using rclone's native Google Drive backend (gdrive remote)
# Check gdrive remote configuration:
rclone config show gdrive
rclone lsf gdrive: --dirs-only

# rclone sync (Windows local path uses \, rclone remote uses /)
# IMPORTANT: this is one-way sync from local to remote, so be sure to 
# use the correct source and destination paths
rclone sync "D:\local_folder" `
  "gdrive:remote_folder" `
  --progress --update --checksum