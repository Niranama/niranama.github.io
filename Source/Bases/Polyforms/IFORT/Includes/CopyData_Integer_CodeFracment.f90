            ! check for errors
            IF (.NOT.SAME_TYPE_AS(OutArr, InArr)) THEN
                CALL Handle_ErrLevel('CopyData_Integer', ModName, ErrSevere, &
                    'Types of input and output are NOT the same.')
                ErrStat = TrueVal
                RETURN
            END IF
            SELECT TYPE (OutArr)
            TYPE IS (tSInt64)
                SELECT TYPE (InArr)
                TYPE IS (tSInt64)
                    OutArr = InArr
                END SELECT
            TYPE IS (tSInt32)
                SELECT TYPE (InArr)
                TYPE IS (tSInt32)
                    OutArr = InArr
                END SELECT
            TYPE IS (tSInt16)
                SELECT TYPE (InArr)
                TYPE IS (tSInt16)
                    OutArr = InArr
                END SELECT
            TYPE IS (tSInt8)
                SELECT TYPE (InArr)
                TYPE IS (tSInt8)
                    OutArr = InArr
                END SELECT
            END SELECT