            ! check for errors
            IF (.NOT.SAME_TYPE_AS(OutArr, InArr)) THEN
                CALL Handle_ErrLevel('CopyData_Real', ModName, ErrSevere, &
                    'Types of input and output are NOT the same.')
                ErrStat = TrueVal
                RETURN
            END IF
            SELECT TYPE (OutArr)
            TYPE IS (tSingle)
                SELECT TYPE (InArr)
                TYPE IS (tSingle)
                    OutArr = InArr
                END SELECT
            TYPE IS (tDouble)
                SELECT TYPE (InArr)
                TYPE IS (tDouble)
                    OutArr = InArr
                END SELECT
            TYPE IS (tQuad)
                SELECT TYPE (InArr)
                TYPE IS (tQuad)
                    OutArr = InArr
                END SELECT
            END SELECT