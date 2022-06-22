# Dropbox Library for Delphi
Basic Dropbox library for Delphi (VLC and FMX)

![Test application screenshot](/DropboxTest/Img/DropboxTest.png?raw=true "Test application screenshot")

This library use TEdgeBrowser component instead of TWebBrowser, so you have to install it first:
[https://blog.marcocantu.com/blog/2020-may-edge-browser-component.html](https://blog.marcocantu.com/blog/2020-may-edge-browser-component.html)
and don't forget to copy WebView2Loader.dll file to your app folder. You can find this file in "c:\Program Files (x86)\Embarcadero\Studio\21.0\Redist\win32\" and "c:\Program Files (x86)\Embarcadero\Studio\21.0\Redist\win64\" folders.

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

Library use **Personal account** and hasn't been tested with Business account.
Library expects properly configured application on Dropbox account with parameters:

 * "Permission type" = "App folder"
 * "Allow implicit grant" = "Allow"
 * "Redirect URIs" = "http://localhost"
Also don't forget to change permissions in "Permissions" tab before you sign in first time (before you retrieve OAuth2 token).

You can manage your Dropbox applications on [Dropbox App console](https://www.dropbox.com/developers/apps).
