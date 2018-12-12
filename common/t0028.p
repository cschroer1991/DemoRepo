/* t0028.p */

define temp-table gttAppMsg no-undo like appMsg.

define output parameter table for gttAppMsg.

for each appMsg no-lock:
    create gttAppMsg.

    buffer-copy appMsg to gttAppMsg.

end.
