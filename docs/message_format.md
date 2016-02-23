# Whatels Message Format

    Flip := msg-name + space (\20) + byte_size(Payload) + line feed (\r) + new line (\n)
    Flop := Payload + line feed (\r) + new line (\n)
    Message := Flip + Flop

