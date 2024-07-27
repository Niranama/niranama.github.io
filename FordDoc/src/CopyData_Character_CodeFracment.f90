            ! check for errors
            IF (.NOT.SAME_TYPE_AS(OutArr, InArr)) THEN
                CALL Handle_ErrLevel('CopyData_Character', ModName, ErrSevere, &
                    'Types of input and output are NOT the same.')
                ErrStat = TrueVal
                RETURN
            END IF
            SELECT TYPE (OutArr)
            TYPE IS (tCharStar)
                SELECT TYPE (InArr => InArr)
                TYPE IS (tCharStar)
                    IF (LEN(OutArr) /= LEN(InArr)) THEN
                        CALL Handle_ErrLevel('CopyData_Character', ModName, ErrSevere, &
                            'Lengths of input and output are NOT the same.')
                        ErrStat = TrueVal
                    ELSE
                        OutArr = InArr
                    END IF
                END SELECT
            END SELECT
