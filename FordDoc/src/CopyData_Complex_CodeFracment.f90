            ! check for errors
            IF (.NOT.SAME_TYPE_AS(OutArr, InArr)) THEN
                CALL Handle_ErrLevel('CopyData_Complex', ModName, ErrSevere, &
                    'Types of input and output are NOT the same.')
                ErrStat = TrueVal
                RETURN
            END IF
            SELECT TYPE (OutArr)
            TYPE IS (tCmpxSingle)
                SELECT TYPE (InArr)
                TYPE IS (tCmpxSingle)
                    OutArr = InArr
                END SELECT
            TYPE IS (tCmpxDouble)
                SELECT TYPE (InArr)
                TYPE IS (tCmpxDouble)
                    OutArr = InArr
                END SELECT
            TYPE IS (tCmpxQuad)
                SELECT TYPE (InArr)
                TYPE IS (tCmpxQuad)
                    OutArr = InArr
                END SELECT
            END SELECT