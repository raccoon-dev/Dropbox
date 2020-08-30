# Dropbox Library for Delphi
Basic Dropbox library for Delphi (VLC and FMX)

![Test application screenshot](/DropboxTest/Img/DropboxTest.png?raw=true "Test application screenshot")

Supported operations:

 * Create folder **/2/files/create_folder_v2**
 * Delete **/2/files/delete_v2**
 * Download **/2/files/download**
 * Download zip **/2/files/download_zip**
 * List folder **/2/files/list_folder**
 * Move **/2/files/move_v2**
 * Upload **/2/files/upload**
 * List Shared Links **/2/sharing/list_shared_links**
 * Delete Shared Link **/2/sharing/revoke_shared_link**
 * Create Shared Link **/2/sharing/create_shared_link_with_settings**

Before compile test application, change **APP_KEY** constant to your App key from your Dropbox account.

Library use **Personal account** and never has been tested with Business account.
Library expects properly configured application on Dropbox account with parameters:

 * "Permission type" = "App folder"
 * "Allow implicit grant" = "Allow"
 * "Redirect URIs" = "http://localhost"

You can manage your Dropbox applications on [Dropbox App console](https://www.dropbox.com/developers/apps).
