            ! check for errors
            IF (.NOT.SAME_TYPE_AS(OutArr, InArr)) THEN
                CALL Handle_ErrLevel('CopyData_Logical', ModName, ErrSevere, &
                    'Types of input and output are NOT the same.')
                ErrStat = TrueVal
                RETURN
            END IF
            SELECT TYPE (OutArr)
            TYPE IS (tLogical)
                SELECT TYPE (InArr)
                TYPE IS (tLogical)
                    OutArr = InArr
                END SELECT
            END SELECT