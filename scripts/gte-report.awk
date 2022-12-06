# Produce a test report from the output of
# ps1-tests/gte/test-all.
# Use like this:
# awk -f gte-report.awk outfile | sort -n

BEGIN {
    name[""] = "Registers"
    name["0x01"] = "RTPS"
    name["0x06"] = "NCLIP"
    name["0x0c"] = "OP"
    name["0x10"] = "DCPS"
    name["0x11"] = "INTPL"
    name["0x12"] = "MVMVA"
    name["0x13"] = "NCDS"
    name["0x14"] = "CDP"
    name["0x16"] = "NCDT"
    name["0x1b"] = "NCCS"
    name["0x1c"] = "CC"
    name["0x1e"] = "NCS"
    name["0x20"] = "NCT"
    name["0x28"] = "SQR"
    name["0x29"] = "DCPL"
    name["0x2a"] = "DPCT"
    name["0x2d"] = "AVSZ3"
    name["0x2e"] = "AVSZ4"
    name["0x30"] = "RTPT"
    name["0x3d"] = "GPF"
    name["0x3e"] = "GPL"
    name["0x3f"] = "NCCT"
}

/Assertion passed/ {
    passed[$7]++
}

/Assertion failed/ {
    failed[$7]++
}

END {
    for (test in passed) {
        if (!(test in failed)) {
            failed[test] = 0
        }
    }

    for (test in failed) {
        if (!(test in passed)) {
            passed[test] = 0
        }
    }

    for (test in passed) {
        percent = (passed[test]/(passed[test]+failed[test])*100)
        test_name = test in name ? name[test] : test
        printf "%3.0f%% %s (%d passed, %d failed)\n",
                  percent, test_name, passed[test], failed[test]
    }
}
