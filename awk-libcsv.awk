
function csv_parse(string,csv,sep,quote,escape,newline,trim, fields,pos,strtrim) {
    # Make sure there is something to parse.
    if (length(string) == 0) return 0
    # Initial setup.
    string = sep string # The code below assumes ,FIELD.
    fields = 0          # The number of fields found thus far.
    # The main parsing loop.
    while (length(string) > 0) {
        # Remove spaces after the separator if requested.
        if (trim && substr(string, 2, 1) == " ") {
            if (length(string) == 1) return fields
            string = substr(string, 2)
            continue
        }
        strtrim = 0 # Used to trim quotes off strings.
        # Handle a quoted field.
        if (substr(string, 2, 1) == quote) {
            pos = 2
            do {
                pos++
                if (pos != length(string) &&
                    substr(string, pos, 1) == escape &&
                    index(quote escape, substr(string, pos + 1, 1)) != 0) {
                    # Remove escaped quote and escape characters.
                    string = substr(string, 1, pos - 1) substr(string, pos + 1)
                } else if (substr(string, pos, 1) == quote) {
                    # Found the end of the string.
                    strtrim = 1
                } else if (pos >= length(string)) {
                    # Handle embedded newlines if requested.
                    if (newline == -1) {
                        return -1
                    } else if (newline) {
                        if (getline == -1) return -4
                        string = string newline $0
                    }
                }
            } while (pos < length(string) && strtrim == 0)
            # Make sure the end of the string is found.
            if (strtrim == 0) {
                return -3
            }
        } else {
            # Handle an empty field.
            if (length(string) == 1 || substr(string, 2, 1) == sep) {
                fields++
                csv[fields] = ""
                if (length(string) == 1) return fields
                string = substr(string, 2)
                continue
            }
            # Search for a separator.
            pos = index(substr(string, 2), sep)
            # If there is no separator the rest of the string is a field.
            if (pos == 0) {
                fields++
                csv[fields] = substr(string, 2)
                return fields
            }
        }
        # Remove spaces after the separator if requested.
        if (trim && pos != (length(string) + strtrim) && substr(string, pos + strtrim, 1) == " ") {
            trim = strtrim
            # Count the number fo spaces found.
            while (pos < length(string) && substr(string, pos + trim, 1) == " ") {
                trim++
            }
            # Remove them from the string.
            string = substr(string, 1, pos + strtrim - 1) substr(string,  pos + trim)
            # Adjust pos with the trimmed spaces if a quotes string was not found.
            if (!strtrim) {
                pos -= trim
            }
        }
        # Make sure we are at the end of the string or there is a separator.
        if ((pos != length(string) && substr(string, pos + 1, 1) != sep)) {
            return -4
        }
        # Gather the field.
        fields++
        csv[fields] = substr(string, 2 + strtrim, pos - (1 + strtrim * 2))
        # Remove the field from the string for the next pass.
        if (pos == length(string)) {
            return fields
        } else {
            string = substr(string, pos + 1)
        }
    }
    return fields
}
