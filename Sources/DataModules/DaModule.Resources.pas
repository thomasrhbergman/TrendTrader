unit DaModule.Resources;

interface

resourcestring
  C_SQL_IS_EXISTS_FIELD = 'select 1 from RDB$RELATION_FIELDS rf where rf.RDB$RELATION_NAME = :TableName ' +
                           'and rf.RDB$FIELD_NAME = :FieldName';
  C_SQL_GET_DOMEN = 'select f.rdb$field_length, f.rdb$field_name, f.rdb$field_type, f.rdb$field_sub_type, ' +
                    ' f.rdb$field_precision, f.rdb$field_scale, rdb$segment_length' +
                    '  from rdb$relation_fields r, rdb$fields f   ' +
                    '    left outer join rdb$character_sets ch on (ch.rdb$character_set_id = f.rdb$character_set_id)' +
                    ' where r.rdb$field_source = f.rdb$field_name ' +
                    '   and r.rdb$field_name = :FieldName         ' +
                    '   and r.rdb$relation_name = :TableName      ';
  C_SQL_DOMEN_EDIT = 'update rdb$fields                     ' +
                     '   set rdb$field_length = :Length,    ' +
                     '       rdb$character_length = :Length ' +
                     ' where rdb$field_name = :FieldName    ';
  C_SQL_DOMEN_TYPE_EDIT = ' update rdb$fields                  ' +
                          '    set rdb$field_type = :FieldType ' +
                          '  where rdb$field_name = :FieldName ';

  C_SQL_IS_EXISTS_TABLE       = 'select 1 from rdb$relations where rdb$relation_name = :ObjectName';
  C_SQL_IS_EXISTS_PROCEDURE   = 'select 1 from rdb$procedures where rdb$procedure_name = :ObjectName';
  C_SQL_IS_EXISTS_FUNCTIONS   = 'select 1 from rdb$functions where rdb$function_name = :ObjectName';
  C_SQL_IS_EXISTS_DOMAIN      = 'select 1 from rdb$fields where rdb$field_name = :ObjectName';
  C_SQL_IS_EXISTS_FOREIGN_KEY = 'select 1 from rdb$relation_constraints where rdb$constraint_name = :ObjectName';

  C_SQL_DROP_TABLE = 'DROP TABLE %s;';

  C_SQL_CREATE_DOMAIN_D_BOOLEAN = 'CREATE DOMAIN D_BOOLEAN AS SMALLINT ' + sLineBreak +
                                  'CHECK (VALUE IS NULL OR VALUE IN (0, 1)); ';

  C_SQL_UPDATE_DOMAIN_D_BOOLEAN = 'UPDATE RDB$RELATION_FIELDS SET RDB$FIELD_SOURCE = ''D_BOOLEAN'' ' + sLineBreak +
                                  ' WHERE RDB$FIELD_SOURCE IN '     + sLineBreak +
                                  '       (SELECT F.RDB$FIELD_NAME ' + sLineBreak +
                                  '          FROM RDB$FIELDS F ' + sLineBreak +
                                  '         WHERE F.RDB$FIELD_TYPE = 17) ';

  C_SQL_UDF_EXTERNAL_FUNCTION_SQRT =
                          'DECLARE EXTERNAL FUNCTION SQRT '    + sLineBreak +
                          'DOUBLE PRECISION '                  + sLineBreak +
                          'RETURNS DOUBLE PRECISION BY VALUE ' + sLineBreak +
                          'ENTRY_POINT ''IB_UDF_sqrt'' MODULE_NAME ''ib_udf'' ';

  C_SQL_PROCEDURE_GET_VOLATILITY =
                          'create procedure GET_VOLATILITY ( '                                            + sLineBreak +
                          '    CONID integer, '                                                           + sLineBreak +
                          '    DATE_BEGIN timestamp, '                                                    + sLineBreak +
                          '    DATE_END timestamp) '                                                      + sLineBreak +
                          'returns ( '                                                                    + sLineBreak +
                          '    VOLATILITY double precision) '                                             + sLineBreak +
                          'as '                                                                           + sLineBreak +
                          'declare variable CNT integer; '                                                + sLineBreak +
                          'declare variable SUM_VALUE double precision; '                                 + sLineBreak +
                          'declare variable AVR double precision; '                                       + sLineBreak +
                          'begin '                                                                        + sLineBreak +
                          '  VOLATILITY = 0; '                                                            + sLineBreak +
                          '  select sum(TD.TICK_VALUE) as SUM_VALUE, count(*) as CNT '                    + sLineBreak +
                          '  from TICK_DATA TD '                                                          + sLineBreak +
                          '  where TD.TICK_TYPE in (4, 9) and '                                           + sLineBreak +
                          '        TD.CONID = :CONID and '                                                + sLineBreak +
                          '        TD.TICK_VALUE > 0 and '                                                + sLineBreak +
                          '        TD.TICK_DATE between :DATE_BEGIN and :DATE_END '                       + sLineBreak +
                          '  into :SUM_VALUE, :CNT; '                                                     + sLineBreak +
                          '  if (:CNT - 1 > 0) then '                                                     + sLineBreak +
                          '  begin '                                                                      + sLineBreak +
                          '    AVR = :SUM_VALUE / :CNT; '                                                 + sLineBreak +
                          '    select sum((TD.TICK_VALUE - :AVR) * (TD.TICK_VALUE - :AVR)) as SUM_VALUE ' + sLineBreak +
                          '    from TICK_DATA TD '                                                        + sLineBreak +
                          '    where TD.TICK_TYPE in (4, 9) and '                                         + sLineBreak +
                          '          TD.CONID = :CONID and '                                              + sLineBreak +
                          '          TD.TICK_VALUE > 0 and '                                              + sLineBreak +
                          '          TD.TICK_DATE between :DATE_BEGIN and :DATE_END '                     + sLineBreak +
                          '    into :SUM_VALUE; '                                                         + sLineBreak +
                          '    VOLATILITY = SQRT(:SUM_VALUE / (:CNT - 1)); '                              + sLineBreak +
                          '  end '                                                                        + sLineBreak +
                          '  suspend; '                                                                   + sLineBreak +
                          'end ';

  C_SQL_PROCEDURE_INS_TICK_DATA =
                          'create procedure INS_TICK_DATA ( '                                             + sLineBreak +
                          '    CONID integer, '                                                           + sLineBreak +
                          '    TICKDATE timestamp, '                                                      + sLineBreak +
                          '    TICKTYPE integer, '                                                        + sLineBreak +
                          '    TICKVALUE float, '                                                         + sLineBreak +
                          '    IS_HISTORICAL boolean) '                                                   + sLineBreak +
                          'as '                                                                           + sLineBreak +
                          'declare variable AVG5 float; '                                                 + sLineBreak +
                          'begin '                                                                        + sLineBreak +
                          '  if (:TICKTYPE = 4) then '                                                    + sLineBreak +
                          '  begin '                                                                      + sLineBreak +
                          '    select AVG(TICK_VALUE) from ('                                             + sLineBreak +
                          '      select TICK_VALUE '                                                      + sLineBreak +
                          '      from TICK_DATA '                                                         + sLineBreak +
                          '      where TICK_TYPE = 4 and '                                                + sLineBreak +
                          '            CONID = :CONID and '                                               + sLineBreak +
                          '            IS_HISTORICAL = false '                                            + sLineBreak +
                          '      order by TICK_DATE desc rows 5  '                                        + sLineBreak +
                          '    ) T '                                                                      + sLineBreak +
                          '    into :AVG5; '                                                              + sLineBreak +
                          '  end '                                                                        + sLineBreak +
                          '  else '                                                                       + sLineBreak +
                          '    AVG5 = 0; '                                                                + sLineBreak +

                          '  insert into TICK_DATA (CONID, TICK_DATE, TICK_TYPE, TICK_VALUE, IS_HISTORICAL, AVERAGE5) ' + sLineBreak +
                          '  values (:CONID, :TICKDATE, :TICKTYPE, :TICKVALUE, :IS_HISTORICAL, :AVG5); '                + sLineBreak +
                          'end';

  C_SQL_PROCEDURE_IN_KURS =
                          'create procedure IN_KURS ( '                           + sLineBreak +
                          '    P_TID timestamp, '                                 + sLineBreak +
                          '    P_DBNR smallint, '                                 + sLineBreak +
                          '    P_KURS float) '                                    + sLineBreak +
                          'as '                                                   + sLineBreak +
                          'begin '                                                + sLineBreak +
                          '   INSERT INTO AKTIEKURS (K_TID,K_DBNR,K_KURSEN) '     + sLineBreak +
                          '   VALUES (:"P_TID",:"P_DBNR",:"P_KURS"); '            + sLineBreak +
                          'end';

  C_SQL_PROCEDURE_IN_KURS_OM =
                          'create procedure IN_KURS_OM ( '                                                        + sLineBreak +
                          '    P_TID timestamp, '                                                                 + sLineBreak +
                          '    P_DBNR smallint, '                                                                 + sLineBreak +
                          '    P_KURS float) '                                                                    + sLineBreak +
                          'returns ( '                                                                            + sLineBreak +
                          '    O_RET boolean) '                                                                   + sLineBreak +
                          'as '                                                                                   + sLineBreak +
                          'declare variable ANT smallint; '                                                       + sLineBreak +
                          'begin '                                                                                + sLineBreak +
                          '   SELECT COUNT(*) FROM AKTIEKURS WHERE K_TID=:"P_TID" AND K_DBNR=:"P_DBNR" INTO ANT;' + sLineBreak +
                          '   IF (ANT=0) THEN BEGIN '                                                             + sLineBreak +
                          '      INSERT INTO AKTIEKURS (K_TID,K_DBNR,K_KURSEN) '                                  + sLineBreak +
                          '      VALUES (:"P_TID",:"P_DBNR",:"P_KURS"); '                                         + sLineBreak +
                          '      O_RET=TRUE; '                                                                    + sLineBreak +
                          '   END '                                                                               + sLineBreak +
                          '   ELSE O_RET=FALSE; '                                                                 + sLineBreak +
                          'end ';

  C_SQL_UDF_EXTERNAL_FUNCTION_ABS =
                          'DECLARE EXTERNAL FUNCTION ABS '     + sLineBreak +
                          'DOUBLE PRECISION '                  + sLineBreak +
                          'RETURNS DOUBLE PRECISION BY VALUE ' + sLineBreak +
                          'ENTRY_POINT ''IB_UDF_abs'' MODULE_NAME ''ib_udf''; ';

  C_SQL_PROCEDURE_GET_GRADIENT =
                      'create procedure GET_GRADIENT ( '                                                                                + sLineBreak +
                      '    CONID integer, '                                                                                             + sLineBreak +
                      '    WEEKS integer) '                                                                                             + sLineBreak +
                      'returns ( '                                                                                                      + sLineBreak +
                      '    GRADIENT decimal(15,6), '                                                                                    + sLineBreak +
                      '    CORRIDOR_WIDTH decimal(15,6)) '                                                                              + sLineBreak +
                      'as '                                                                                                             + sLineBreak +
                      'declare variable SUMX float; '                                                                                   + sLineBreak +
                      'declare variable SUMY float; '                                                                                   + sLineBreak +
                      'declare variable SUMXY float; '                                                                                  + sLineBreak +
                      'declare variable SUMSQRX float; '                                                                                + sLineBreak +
                      'declare variable CNT integer; '                                                                                  + sLineBreak +
                      'declare variable AVGX float; '                                                                                   + sLineBreak +
                      'declare variable AVGY float; '                                                                                   + sLineBreak +
                      'declare variable A float; '                                                                                      + sLineBreak +
                      'declare variable B float; '                                                                                      + sLineBreak +
                      'declare variable TIMEFIRST float; '                                                                              + sLineBreak +
                      'declare variable TIMELAST float; '                                                                               + sLineBreak +
                      'declare variable LINEUPY1 float; '                                                                               + sLineBreak +
                      'declare variable LINEUPY2 float; '                                                                               + sLineBreak +
                      'declare variable DELTAP float; '                                                                                 + sLineBreak +
                      'declare variable DELTAT float; '                                                                                 + sLineBreak +
                      'begin '                                                                                                          + sLineBreak +
                      '  GRADIENT = 0; '                                                                                                + sLineBreak +
                      '  CORRIDOR_WIDTH = 0; '                                                                                          + sLineBreak +
                      '  WEEKS = :WEEKS * 7; '                                                                                          + sLineBreak +
                      '  for select count(*) CNT, sum(cast(td.tick_date - cast(''12/30/1900 00:00'' as timestamp) as float)) SUMX, '    + sLineBreak +
                      '             sum(td.tick_value) SUMY, '                                                                          + sLineBreak +
                      '             sum(cast(td.tick_date - cast(''12/30/1900 00:00'' as timestamp) as float) * td.tick_value) SUMXY, ' + sLineBreak +
                      '             sum(SQRT(td.tick_date - cast(''12/30/1900 00:00'' as timestamp))) SUMSQRX '                         + sLineBreak +
                      '      from TICK_DATA td '                                                                                        + sLineBreak +
                      '      where td.conid = :CONID and '                                                                              + sLineBreak +
                      '            td.tick_date between current_timestamp -:WEEKS and current_timestamp and '                           + sLineBreak +
                      '            td.tick_type in (4, 9) and '                                                                         + sLineBreak +
                      '            td.tick_value > 0.01 '                                                                               + sLineBreak +
                      '      into :CNT, :SUMX, :SUMY, :SUMXY, :SUMSQRX  '                                                               + sLineBreak +
                      '  do '                                                                                                           + sLineBreak +
                      '  begin '                                                                                                        + sLineBreak +
                      '    AVGX = :SUMX / :CNT; '                                                                                       + sLineBreak +
                      '    AVGY = :SUMY / :CNT; '                                                                                       + sLineBreak +
                      '    B = ((:CNT * :SUMXY) - (:SUMX * :SUMY)) / ((:CNT * :SUMSQRX) - SQRT(:SUMX)); '                               + sLineBreak +
                      '    A = :AVGY - (:B * AVGX); '                                                                                   + sLineBreak +
                      '  end  '                                                                                                         + sLineBreak +
                      '  select min(cast(td.tick_date - cast(''12/30/1900 00:00'' as timestamp) as float)), '                           + sLineBreak +
                      '         max(cast(td.tick_date - cast(''12/30/1900 00:00'' as timestamp) as float)) '                            + sLineBreak +
                      '  from TICK_DATA td '                                                                                            + sLineBreak +
                      '  where td.conid = :CONID and '                                                                                  + sLineBreak +
                      '        td.tick_date between current_timestamp -:WEEKS and current_timestamp and '                               + sLineBreak +
                      '        td.tick_type in (4, 9) and '                                                                             + sLineBreak +
                      '        td.tick_value > 0.01 '                                                                                   + sLineBreak +
                      '  into :TIMEFIRST, :TIMELAST; '                                                                                  + sLineBreak +
                      '  LINEUPY1 = :A + :B * :TIMEFIRST; '                                                                             + sLineBreak +
                      '  LINEUPY2 = :A + :B * :TIMELAST; '                                                                              + sLineBreak +
                      '  DELTAP = :LINEUPY2 - :LINEUPY1; '                                                                              + sLineBreak +
                      '  DELTAT = :TIMELAST - :TIMEFIRST; '                                                                             + sLineBreak +
                      '  if ((:DELTAP > -0.0000001) and '                                                                               + sLineBreak +
                      '      (:DELTAP < 0.0000001)) then '                                                                              + sLineBreak +
                      '  begin '                                                                                                        + sLineBreak +
                      '    if (:DELTAP * :DELTAT > 0) then '                                                                            + sLineBreak +
                      '      GRADIENT = 100.0; '                                                                                        + sLineBreak +
                      '    else '                                                                                                       + sLineBreak +
                      '      GRADIENT = -100.0; '                                                                                       + sLineBreak +
                      '  end '                                                                                                          + sLineBreak +
                      '  else '                                                                                                         + sLineBreak +
                      '    GRADIENT = :DELTAP / :DELTAT; '                                                                              + sLineBreak +
                      '  suspend; '                                                                                                     + sLineBreak +
                      'end';

C_SQL_CLEAN_DATABASE = 'delete from ALGORITMOS; '              + sLineBreak +
                       'delete from AUTOTRADES; '              + sLineBreak +
                       'delete from CONDITION; '               + sLineBreak +
                       'delete from DOC_RELATIONS; '           + sLineBreak +
                       'delete from FACTOR; '                  + sLineBreak +
                       'delete from MARKET_RULES; '            + sLineBreak +
                       'delete from ORDER_GROUP; '             + sLineBreak +
                       'delete from ORDER_GROUP_SET; '         + sLineBreak +
                       'delete from ORDERS; '                  + sLineBreak +
                       'delete from QUALIFIERS; '              + sLineBreak +
                       'delete from QUALIFIERS_CONDITION; '    + sLineBreak +
                       'delete from SCAN_MARKET; '             + sLineBreak +
                       'set generator GEN_DOCUMENT_ID to 0; '  + sLineBreak +
                       'set generator GEN_RELATIONS_ID to 0; ' + sLineBreak +
                       'commit; ';

C_SQL_CREATE_TICK_TYPES = 'CREATE TABLE TICK_TYPES ( '                                   + sLineBreak +
                          '    ID           INTEGER NOT NULL, '                          + sLineBreak +
                          '    NAME         VARCHAR(50) CHARACTER SET NONE NOT NULL, '   + sLineBreak +
                          '    DESCRIPTION  VARCHAR(200) CHARACTER SET NONE '            + sLineBreak +
                          '); '                                                          + sLineBreak +
                          'ALTER TABLE TICK_TYPES ADD CONSTRAINT PK_TICK_TYPES PRIMARY KEY (ID); ';

implementation

end.
