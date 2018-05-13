Updating GT 730 on Archlinux
2018-05-13 12:50:58.335749653 UTC
Post

Let's cut the crap.

Chronology:

1. Updated nvidia driver.
2. Not knowing my card has been legacied.
3. Reboot.
4. ???
5. Sad

Comeback:

1. Investigated boot log.
2. Found out that the card is has been legacied.
   ![alt text](images/nvidia.jpg "this bullshit")
3. Typed painfully on getty because I enabled `graphic.service`. 
4. Disabled `graphic.service`.
5. Reboot.
6. Uninstalled `nvidia` and `nvidia-utils`.
7. Installed `nvidia-390xx` and `nvidia-390xx`.
8. Start the X server.
9. Watch kitten videos so I am not sad anymore.
