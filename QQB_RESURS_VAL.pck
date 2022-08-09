create or replace package QQB_RESURS_VAL is
  procedure prot(iId_res         qqb_hl_resurs_protocol.id_res%type,
                 iIzox           qqb_hl_resurs_protocol.Izox%type,
                 iState_id       qqb_hl_resurs_protocol.state_id%type,
                 iOp_id          number,
                 iDate_operation qqb_hl_resurs_protocol.date_operation%type);
  procedure prot_index(iCode           qqb_hl_index_protocol.code%type,
                       iIzox           qqb_hl_index_protocol.Izox%type,
                       iState_id       qqb_hl_spr_state_type.id%type,
                       iCondition_id   qqb_hl_index_protocol.condition_id%type,
                       iDate_operation qqb_hl_index_protocol.date_operation%type);
  procedure T3_template(iT2_Id in qqb_hl_spr_submanba.id%type);
  procedure request_add(REQUEST HASHTABLE);

  procedure sorov_tasdiq(iid           glb_cred_hl_summa.id%type,
                         iruhsat_state glb_cred_hl_summa.state%type,
                         isumma        glb_cred_hl_summa.ruhsat_sum%type);
  procedure attach_dep(REQUEST HASHTABLE);
  --//manba add
  procedure manba_add(iName      qqb_hl_spr_manba.name%type,
                      iRevolver  qqb_hl_spr_manba.revolver%type,
                      iCondition qqb_hl_spr_manba.condition%type,
                      oMessage   out varchar2);
  procedure rad_etish(iid   qqb_hl_resurs.id%type,
                      iizoh varchar2,
                      omess out varchar2);
  PROCEDURE LIBOR_ACTION(IACTION        IN VARCHAR2,
                         IDATE_VALIDATE IN qqb_hl_s_libors.DATE_VALIDATE%TYPE,
                         IPERCENT_RATE  IN NUMBER);
  procedure manba_ed(iCode      qqb_hl_spr_manba.code%type,
                     iName      qqb_hl_spr_manba.name%type,
                     iRevolver  qqb_hl_spr_manba.revolver%type,
                     iCondition qqb_hl_spr_manba.condition%type,
                     oMessage   out varchar2);
  procedure manba_active_disactive(iCode qqb_hl_spr_manba.code%type);
  procedure detail_add(iManbaCode qqb_hl_spr_submanba.manba_code%type,
                       iName      qqb_hl_spr_submanba.name%type,
                       iSchet     qqb_hl_spr_submanba.schet%type,
                       iProc      qqb_hl_spr_submanba.foiz%type,
                       iType      qqb_hl_spr_submanba.type%type,
                       iCurr      qqb_hl_spr_submanba.currency%type,
                       oMessage   out varchar2);
  procedure detail_ed(iId       qqb_hl_spr_submanba.id%type,
                      iName     qqb_hl_spr_submanba.name%type,
                      iSchet    qqb_hl_spr_submanba.schet%type,
                      iProc     qqb_hl_spr_submanba.foiz%type,
                      iType     qqb_hl_spr_submanba.type%type,
                      iCurrency qqb_hl_spr_submanba.currency%type,
                      oMessage  out varchar2);
  procedure detail_del(iId qqb_hl_spr_submanba.id%type);
  procedure index_add(iName      qqb_hl_indexes.name%type,
                      iCode_type qqb_hl_indexes.code_type%type,
                      oMessage   out varchar2);
  procedure index_ed(iId      qqb_hl_indexes.id%type,
                     iName    qqb_hl_indexes.name%type,
                     oMessage out varchar2);
  procedure index_info_add(iIndex_Id   qqb_hl_index_info.index_id%type,
                           iBegin_date qqb_hl_index_info.begin_date%type,
                           iEnd_date   qqb_hl_index_info.end_date%type,
                           iProcent    qqb_hl_index_info.procent%type,
                           oMessage    out varchar2);
  procedure index_info_ed(iId         qqb_hl_index_info.id%type,
                          iBegin_date qqb_hl_index_info.begin_date%type,
                          iEnd_date   qqb_hl_index_info.end_date%type,
                          iProcent    qqb_hl_index_info.procent%type,
                          oMessage    out varchar2);
  procedure detail_filial_ed(iId           qqb_hl_accounts.t2_id%type,
                             iFilial       qqb_hl_accounts.filial%type,
                             iAcc_res_bo   qqb_hl_accounts.acc_res_bo%type,
                             iAcc_foiz_bo  qqb_hl_accounts.acc_foiz_bo%type,
                             iAcc_res_fil  qqb_hl_accounts.acc_res_fil%type,
                             iAcc_foiz_fil qqb_hl_accounts.acc_foiz_fil%type,
                             oMessage      out varchar2);
  procedure percent_rate_synch(iIndex_id qqb_hl_indexes.id%type);
  function is_invalid_date(iId qqb_hl_index_info.id%type) return number;
  function Get_Sch_Name(iSch accounts.acc_external%type) return varchar2;
  function get_proc_acc(iMfo accounts.code_filial%type,
                        iAcc accounts.acc_external%type) return varchar2;
  function Get_Dep_Name(iDep_id dep_contracts.id%type) return varchar2;
  function Get_Dep_Name2(iDep_id dep_contracts.id%type) return varchar2;
  function Get_manba_Name(iZv_id glb_cred_zayav.id%type,
                          iType  number default 0) return varchar2;
  function Get_manba_Name1(iCode qqb_hl_spr_manba.code%type) return varchar2;
  function Get_manba_Name2(iCode qqb_hl_spr_manba.code%type) return varchar2;
  function Get_manba_Name3(iId qqb_hl_spr_submanba.id%type, iDate date) return varchar2;
  procedure SendToMain(iId_res qqb_hl_resurs.id%type);
  procedure generator_graph(iLoan_id                ln_graph_debt.loan_id%type,
                            iDep_contract_id        dep_contracts.id%type,
                            IDep_filial_contract_id dep_contracts.id%type);
  function chek_schedules_graph(iDep_contract_id dep_contracts.id%type)
    return number;
  --------------------------------------
  ----------------Azizbek---------------
  --------------------------------------
  Procedure accept(i_submanba_id           number,
                   i_contract_name         dep_contracts.contract_name%type,
                   i_contract_number       dep_contracts.contract_number%type,
                   i_contract_date         varchar2,
                   i_date_begin            varchar2,
                   i_date_end              varchar2,
                   i_percent               DEP_CONTRACTS_PERCENT_RATE.PERCENT_RATE%type,
                   i_percent_DATE_VALIDATE varchar2,
                   i_desc_days_in_year     DEP_s_DAYS_IN_YEAR.Code%type,
                   i_zv_id                 number,
                   i_resurs_id             qqb_hl_resurs.id%type,
                   o_Message               out varchar2);

  --------------------------------------
  -----------------Shams----------------
  --------------------------------------
  procedure createAccounts(iT2_ID      varchar2,
                           iFilial     varchar2,
                           iClientName varchar2,
                           oCondition  out varchar2,
                           oDialog     out varchar2,
                           oMessage    out varchar2);
  --Nazarox X.A  28.09.2020 h/r biriktirish
  procedure openBindAcc(iMfo        varchar2,
                        iSubmanbaId integer,
                        b_id        dep_contracts.id%type,
                        f_id        dep_contracts.id%type,
                        oMessage    out varchar2);
  --Nazarov X.A 28.09.2020
  procedure provodka(s_id glb_cred_hl_summa.id%type, oMessage out varchar2);
  /*Procedure accept_test;*/
  procedure openBindAcc_TEST(iMfo        varchar2,
                             iSubmanbaId integer,
                             b_id        dep_contracts.id%type,
                             f_id        dep_contracts.id%type,
                             oMessage    out varchar2,
                             tartibNo    varchar2);
----------------------Fozil-----------------------------------------
  procedure far_add(iSon      qqb_hl_spr_farmoyish.far_numb%type,
                    iSana     qqb_hl_spr_farmoyish.far_date%type,
                    oMessage   out varchar2);
end QQB_RESURS_VAL;
/
create or replace package body qqb_resurs_val is
  vResId qqb_hl_resurs.id%type;

  vState_id   qqb_hl_spr_state_type.id%type;
  vState_name qqb_hl_spr_state_type.name%type;
  -- vOp_id          number;
  vCode_filial accounts.code_filial%type;
  --vIzox           qqb_hl_resurs.izox%type;
  vDate           qqb_hl_resurs.resurs_date%type;
  vDate_operation qqb_hl_resurs_protocol.date_operation%type;
  vZvId           glb_cred_zayav.id%type;
  -- vNumber         qqb_hl_resurs.resurs_number%type;
  vSumma     qqb_hl_resurs.summa%type;
  vMuddat    qqb_hl_resurs.muddat%type;
  vIndexName qqb_hl_spr_proc_type.name%type;
  vCondition qqb_hl_spr_proc_type.condition%type;
  vSchName   accounts.name%type;
  vAcc       accounts.acc_external%type;
  vCount     number;
  vResRow    qqb_hl_resurs%rowtype;
  vZvRow     glb_cred_zayav%rowtype;
  vCurrency  qqb_hl_spr_submanba.currency%type;
  vText      qqb_hl_resurs_protocol.text%type;
  vIdxInfRow qqb_hl_index_info%rowtype;

  --//генератор протокола
  procedure prot(iId_res         qqb_hl_resurs_protocol.id_res%type,
                 iIzox           qqb_hl_resurs_protocol.Izox%type,
                 iState_id       qqb_hl_resurs_protocol.state_id%type,
                 iOp_id          number,
                 iDate_operation qqb_hl_resurs_protocol.date_operation%type) is
    --vText       qqb_hl_resurs_protocol.text%type;
  begin
    --vCode_filial := setup.get_filial_code;
    --vEmp_code    := setup.Get_Employee_Code;
    begin
      if iOp_id = 4 then
        vtext := 'Рад этилди';
      end if;
      insert into qqb_hl_resurs_protocol
        (id_res,
         code_filial,
         emp_code,
         date_operation,
         text,
         state_id,
         izox)
      values
        (iId_res,
         setup.get_filial_code,
         setup.get_employee_code,
         iDate_operation,
         vText,
         iState_id,
         iIzox);
    exception
      when others then
        Raise_Application_Error(-20000, sqlerrm);
    end;
  end prot;

  procedure prot_index(iCode           qqb_hl_index_protocol.code%type,
                       iIzox           qqb_hl_index_protocol.Izox%type,
                       iState_id       qqb_hl_spr_state_type.id%type,
                       iCondition_id   qqb_hl_index_protocol.condition_id%type,
                       iDate_operation qqb_hl_index_protocol.date_operation%type) is
    vEmp_code varchar2(8);
  begin
    vCode_filial := setup.Get_Filial_Code;
    vEmp_code    := setup.Get_Employee_Code;
    select s.name
      into vState_name
      from qqb_hl_spr_state_type s
     where s.id = iState_id;
    select i.name
      into vIndexName
      from qqb_hl_spr_proc_type i
     where i.code = iCode;
    vText := vState_name || ' показатель ' || vIndexName;
    insert into qqb_hl_index_protocol
      (code, emp_code, date_operation, text, condition_id, izox)
    values
      (iCode, vEmp_code, iDate_operation, vText, iCondition_id, iIzox);
  end prot_index;

  --//T3 шаблон
  procedure T3_template(iT2_Id in qqb_hl_spr_submanba.id%type) is
    vAcc_res_bo   qqb_hl_accounts.acc_res_bo%type := '16102';
    vAcc_foiz_bo  qqb_hl_accounts.acc_foiz_bo%type := '16304';
    vAcc_res_fil  qqb_hl_accounts.acc_res_fil%type := '22203';
    vAcc_foiz_fil qqb_hl_accounts.acc_foiz_fil%type := '22409';
  begin
    if iT2_Id is not null then
      select t2.currency
        into vCurrency
        from qqb_hl_spr_submanba t2
       where t2.id = iT2_Id;
      if vCurrency is not null then
        for f in (select t.code
                    from bank_desc_glb t
                   where t.code not in ('00000', '00429', '09009')
                     and t.code not in
                         (select c.filial
                            from qqb_hl_accounts c
                           where c.t2_id = iT2_Id)) loop
          insert into qqb_hl_accounts
            (t2_id,
             filial,
             acc_res_bo,
             acc_foiz_bo,
             acc_res_fil,
             acc_foiz_fil)
          values
            (iT2_Id,
             f.code,
             vAcc_res_bo || vCurrency || '____________',
             vAcc_foiz_bo || vCurrency || '____________',
             vAcc_res_fil || vCurrency || '____________',
             vAcc_foiz_fil || vCurrency || '____________');
        end loop;
      else
        Raise_Application_Error(-20000,
                                'Валюта коди киритилмаган!!!');
      end if;
    else
      Raise_Application_Error(-20000, 'ID йук');
    end if;
  end T3_template;

  --//генератор запроса
  ------------------------------------______________
  ---__Shaxzod qoshgan___
  --------------------------------------

  procedure request_add(REQUEST HASHTABLE) is
    vmfo       varchar2(5);
    v_loan_id  ln_card.loan_id%type;
    v_currency ln_card.currency%type;
    --
    vResId          qqb_hl_resurs.id%type;
    vState_id       qqb_hl_spr_state_type.id%type;
    vDate_operation qqb_hl_resurs_protocol.date_operation%type;
    vZvId           glb_cred_zayav.id%type;
    vSumma          qqb_hl_resurs.summa%type;
    vSumma_hl       glb_cred_hl_summa.summa%type;
  
    vMuddat qqb_hl_resurs.muddat%type;
    vId     glb_cred_hl_summa.id%type;
    vDate   date;
    vcount  number;
    --- 
  begin
  
    vDate           := request.get_varchar2('sana');
    vZvId           := request.Get_number('zv_id');
    vId             := request.Get_number('id');
    vSumma_hl       := request.Get_number('summa');
    vState_id       := 0; --отправлен в ГО
    vDate_operation := sysdate;
  
    select count(*) into vcount from qqb_hl_resurs a where a.zv_id = vZvId;
    if vcount = 0 then
      select t.zayav_summa, t.srok, t.mfo, t.loan_id
        into vSumma, vMuddat, vmfo, v_loan_id
        from glb_cred_zayav t
       where t.id = vZvId;
      if v_loan_id is null then
        Raise_Application_Error(-20000,
                                'Кредит бюртмасига loan_id бириктирилмаган?!');
      end if;
      select t.currency
        into v_currency
        from ln_card t
       where t.loan_id = v_loan_id;
      if v_currency = '000' then
        Raise_Application_Error(-20000, ' Валюта эмас ?!');
      end if;
      begin
        vResId := qqb_hl_resurs_seq.nextval;
        insert into qqb_hl_resurs
          (id,
           resurs_date,
           resurs_number,
           summa,
           zv_id,
           submanba_id,
           state,
           modified_date,
           muddat,
           izox,
           filial_code)
        values
          (vResId,
           vDate,
           '',
           vSumma,
           vZvId,
           null,
           vState_id,
           vDate_operation,
           vMuddat,
           '',
           vmfo);
      exception
        when dup_val_on_index then
          Raise_Application_Error(-20000,
                                  vZvId || ': Bu zayavka junatilgan');
      end;
    else
      select a.id into vResId from qqb_hl_resurs a where a.zv_id = vZvId;
    end if;
    begin
      if vId = 0 then
        insert into glb_cred_hl_summa
          (id, id_zayav, data, summa, state, id_resurs)
        values
          (glb_cred_hl_summa_seq.nextval,
           vZvId,
           vDate,
           vSumma_hl,
           0,
           vResId);
      else
        update glb_cred_hl_summa d
           set d.data = vDate, d.summa = vSumma_hl
         where d.id = vId;
      end if;
    exception
      when others then
        Raise_Application_Error(-20000, sqlerrm);
    end;
  
  end request_add;
  -----------------------------------------------------------------------------
  procedure sorov_tasdiq(iid           glb_cred_hl_summa.id%type,
                         iruhsat_state glb_cred_hl_summa.state%type,
                         isumma        glb_cred_hl_summa.ruhsat_sum%type) is
  vIdList           array_varchar2 := Array_Varchar2();
  vCred_hl_summa    glb_cred_hl_summa%rowtype;
  vHl_resurs        qqb_hl_resurs%rowtype;
  vCode_o           core_users.user_id%type;
  vCode_b           core_users.user_id%type;
  oMessage          varchar2(1000);
  begin
    begin
      update glb_cred_hl_summa h
         set h.ruhsat_sum = isumma, h.state = iruhsat_state
       where h.id = iid;
      if iruhsat_state = 1 then
        select *
          into vCred_hl_summa
          from glb_cred_hl_summa a
         where a.id = iid;
        select * into vHl_resurs from qqb_hl_resurs r where r.id = vCred_hl_summa.Id_Resurs;
        select setup.get_employee_code into vCode_o from dual;
        select p.code
          into vCode_b
          from v_emp_pass p
         where p.filial_code = '01037'
           and p.rank_code = 4
           and p.condition = 'A';
        setup.set_employee_code(vCode_b);
        vIdList.extend;
        vIdList(1) := vHl_resurs.Dep_Id_b;
        Dep_Api.contract_action(omessage => oMessage,
                                iaction => 'APPROVE',
                                iid_list => vIdList);
        select p.code
          into vCode_b
          from v_emp_pass p
         where p.filial_code = vHl_resurs.Filial_Code
           and p.rank_code = 4
           and p.condition = 'A';
        setup.set_employee_code(vCode_b);
        vIdList.Delete;
        vIdList.extend;
        vIdList(1) := vHl_resurs.Dep_Id_b;
        Dep_Api.contract_action(omessage => oMessage,
                                iaction => 'APPROVE',
                                iid_list => vIdList);
        setup.set_employee_code(vCode_o);
      end if;
    exception
      when others then
        Raise_Application_Error(-20000, sqlerrm);
    end;
  end;

  procedure rad_etish(iid   qqb_hl_resurs.id%type,
                      iizoh varchar2,
                      omess out varchar2) is
  begin
    update qqb_hl_resurs a
       set a.izox = iizoh, a.state = 4
     where a.id = iid;
    omess := 'OK';
    prot(iId_res         => iid,
         iIzox           => iizoh,
         iState_id       => 4,
         iOp_id          => '4',
         iDate_operation => sysdate);
  
  end;
  ---libor action
  PROCEDURE LIBOR_ACTION(IACTION        IN VARCHAR2,
                         IDATE_VALIDATE IN qqb_hl_s_libors.DATE_VALIDATE%TYPE,
                         IPERCENT_RATE  IN NUMBER) IS
    EX EXCEPTION;
    VMESSAGE      VARCHAR2(4000);
    VSYSDATE      DATE;
    VEMP          qqb_hl_s_libors.EMP_CODE%TYPE;
    VID           NUMBER(5);
    v_date_active date := IDATE_VALIDATE;
    v_rate        number(2) := IPERCENT_RATE;
  BEGIN
  
    VEMP     := SETUP.GET_EMPLOYEE_CODE;
    VSYSDATE := SYSDATE;
  
    IF IDATE_VALIDATE IS NULL THEN
      VMESSAGE := 'дата киритилмаган';
      RAISE EX;
    END IF;
    BEGIN
    
      IF IACTION = 'ENTER' THEN
        VID := qqb_hl_s_libors_seq.nextval;
        BEGIN
          INSERT INTO qqb_hl_s_libors
            (DATE_VALIDATE, PERCENT_RATE, EMP_CODE, DATE_MODIFY, id)
          VALUES
            (IDATE_VALIDATE,
             NVL(IPERCENT_RATE, 0),
             SETUP.GET_EMPLOYEE_CODE,
             VSYSDATE,
             VID);
        
        EXCEPTION
          WHEN DUP_VAL_ON_INDEX THEN
            VMESSAGE := 'Киритилган ' ||
                        TO_CHAR(IDATE_VALIDATE, DEP_CONST.MASKDATE);
            RAISE EX;
        END;
      
      ELSIF IACTION = 'UPDATE' THEN
        select A.ID
          INTO vid
          from qqb_hl_s_libors A
         WHERE A.DATE_VALIDATE = IDATE_VALIDATE;
        UPDATE qqb_hl_s_libors T
           SET T.PERCENT_RATE = NVL(IPERCENT_RATE, 0),
               T.EMP_CODE     = VEMP,
               T.DATE_MODIFY  = VSYSDATE
         WHERE T.DATE_VALIDATE = IDATE_VALIDATE;
      
      ELSIF IACTION = 'DELETE' THEN
        /* UPDATE qqb_hl_s_libors T
              SET T.EMP_CODE = VEMP, T.DATE_MODIFY = VSYSDATE
            WHERE T.DATE_VALIDATE = IDATE_VALIDATE;
        */
        DELETE qqb_hl_s_libors T WHERE T.DATE_VALIDATE = IDATE_VALIDATE;
      ELSE
        RAISE EX;
      END IF;
      ----kredit podsistemasiga qoshish
      ln_kernel.save_interest_rate(i_code        => 'LBR',
                                   i_date_active => v_date_active,
                                   i_rate        => v_rate);
    
      ----depozit yur litsa podsistemasiga qoshish;
      dep_setting.LIBOR_ACTION(iaction        => IACTION,
                               idate_validate => IDATE_VALIDATE,
                               ipercent_rate  => IPERCENT_RATE);
      -- LoanId аникланиш керак
      /*    fozil_actions.ln_percent_update(iLoanId => ,
                                          iTypeAction => 'OLD' );
      */
    EXCEPTION
      WHEN EX THEN
        RAISE_APPLICATION_ERROR(-20000, VMESSAGE);
    END;
  
    insert into qqb_hl_libor_his
      (date_validate,
       percent_rate,
       emp_code,
       date_modify,
       action,
       id_libor)
    values
      (IDATE_VALIDATE,
       NVL(IPERCENT_RATE, 0),
       setup.get_employee_code,
       VSYSDATE,
       IACTION,
       VID);
  END LIBOR_ACTION;

  -------------------------------------------------------------------
  ------------------------------------------------------------------

  --//Деп Юр Лиц бириктириш
  procedure attach_dep(REQUEST HASHTABLE) is
  begin
    vResRow.Id            := request.Get_number('res_id');
    vResRow.Submanba_Id   := request.Get_number('submanba_id');
    vResRow.Dep_Id_b      := request.Get_number('dep_id_b');
    vResRow.Dep_Id_f      := request.Get_number('dep_id_f');
    vResRow.Izox          := request.Get_varchar2('izox');
    vResRow.Modified_Date := sysdate;
    update qqb_hl_resurs t
       set t.submanba_id = vResRow.Submanba_Id,
           t.dep_id_b    = vResRow.Dep_Id_b,
           t.dep_id_f    = vResRow.Dep_Id_f
     where t.id = vResRow.Id;
    prot(vResRow.Id, vResRow.Izox, 2, 3, vResRow.Modified_Date);
  end attach_dep;
  --//manba add
  procedure manba_add(iName      qqb_hl_spr_manba.name%type,
                      iRevolver  qqb_hl_spr_manba.revolver%type,
                      iCondition qqb_hl_spr_manba.condition%type,
                      oMessage   out varchar2) is
    vCode qqb_hl_spr_manba.code%type := qqb_hr_spr_manba_seq.nextval;
  begin
    select count(*)
      into vCount
      from qqb_hl_spr_manba g
     where g.name = iName;
    if vCount = 0 then
      insert into qqb_hl_spr_manba
        (code, name, revolver, condition)
      values
        (vCode, iName, iRevolver, iCondition);
      oMessage := 'OK';
    else
      oMessage := 'No';
    end if;
  end manba_add;
  --//manba edit
  procedure manba_ed(iCode      qqb_hl_spr_manba.code%type,
                     iName      qqb_hl_spr_manba.name%type,
                     iRevolver  qqb_hl_spr_manba.revolver%type,
                     iCondition qqb_hl_spr_manba.condition%type,
                     oMessage   out varchar2) is
  begin
    select count(*)
      into vCount
      from qqb_hl_spr_manba g
     where g.name = iName
       and g.code > 50
       and g.code != iCode;
    if vCount = 0 then
      update qqb_hl_spr_manba g
         set g.name      = iName,
             g.revolver  = iRevolver,
             g.condition = iCondition
       where g.code = iCode;
      oMessage := 'OK';
    else
      oMessage := 'error';
    end if;
  end manba_ed;
  --//manba active_disactive
  procedure manba_active_disactive(iCode qqb_hl_spr_manba.code%type) is
    vCondition qqb_hl_spr_manba.condition%type := 'P';
  begin
    select g.condition
      into vCondition
      from qqb_hl_spr_manba g
     where g.code = iCode;
    if vCondition = 'A' then
      update qqb_hl_spr_manba g set g.condition = 'P' where g.code = iCode;
    else
      update qqb_hl_spr_manba g set g.condition = 'A' where g.code = iCode;
    end if;
  end manba_active_disactive;
  --//qqb_hl_spr_submanba ga kiritish
  procedure detail_add(iManbaCode qqb_hl_spr_submanba.manba_code%type,
                       iName      qqb_hl_spr_submanba.name%type,
                       iSchet     qqb_hl_spr_submanba.schet%type,
                       iProc      qqb_hl_spr_submanba.foiz%type,
                       iType      qqb_hl_spr_submanba.type%type,
                       iCurr      qqb_hl_spr_submanba.currency%type,
                       oMessage   out varchar2) is
    vSubManbaId     qqb_hl_spr_submanba.id%type:=qqb_hl_spr_submanba_seq.nextval;
    vSubManbaVId    qqb_hl_spr_submanba_vneb.id%type:=qqb_hl_spr_submanba_vneb_seq.nextval;
  begin
    select count(*)
      into vCount
      from qqb_hl_spr_submanba t
     where t.name = iName;
    if vCount = 0 then
      insert into qqb_hl_spr_submanba
        (id, manba_code, name, foiz, schet, type, currency)
      values
        (vSubManbaId,
         iManbaCode,
         iName,
         iProc,
         iSchet,
         iType,
         iCurr);
      oMessage := 'OK';
    else
      oMessage := 'No';
    end if;
  end detail_add;
  --//qqb_hl_spr_submanba edit
  procedure detail_ed(iId        qqb_hl_spr_submanba.id%type,
                      iName      qqb_hl_spr_submanba.name%type,
                      iSchet     qqb_hl_spr_submanba.schet%type,
                      iProc      qqb_hl_spr_submanba.foiz%type,
                      iType      qqb_hl_spr_submanba.type%type,
                      iCurrency  qqb_hl_spr_submanba.currency%type,
                      oMessage   out varchar2) is
    vSubManbaVId    qqb_hl_spr_submanba_vneb.id%type;
  begin
    select count(*)
      into vCount
      from qqb_hl_spr_submanba t
     where t.name = iName
       and t.id != iId;
    if vCount = 0 then
      update qqb_hl_spr_submanba t
         set t.name     = iName,
             t.schet    = iSchet,
             t.foiz     = iProc,
             t.type     = iType,
             t.currency = iCurrency
       where t.id = iId;
      oMessage := 'OK';
    else
      oMessage := 'error';
    end if;
  end detail_ed;
  --//qqb_hl_spr_submanba delete
  procedure detail_del(iId qqb_hl_spr_submanba.id%type) is
  begin
    delete from qqb_hl_spr_submanba t where t.id = iId;
  end detail_del;

  procedure index_add(iName      qqb_hl_indexes.name%type,
                      iCode_type qqb_hl_indexes.code_type%type,
                      oMessage   out varchar2) is
    vId qqb_hl_indexes.id%type;
  begin
    select count(*) into vCount from qqb_hl_indexes t where t.name = iName;
    if vCount = 0 then
      vId             := qqb_hl_indexes_seq.nextval;
      vDate_operation := sysdate;
      insert into qqb_hl_indexes
        (id, code_type, name)
      values
        (vId, iCode_type, iName);
      oMessage := 'OK';
    else
      oMessage := 'No';
    end if;
    --prot_index(iCode, iIzox, '5', iCondition, vDate_operation);
  end index_add;

  procedure index_ed(iId      qqb_hl_indexes.id%type,
                     iName    qqb_hl_indexes.name%type,
                     oMessage out varchar2) is
  begin
    select count(*)
      into vCount
      from qqb_hl_indexes t
     where t.name = iName
       and t.id != iId;
    if vCount = 0 then
      vDate_operation := sysdate;
      update qqb_hl_indexes t set t.name = iName where t.id = iId;
      oMessage := 'OK';
      --prot_index(iCode, iIzox, '5', iCondition, vDate_operation);
    else
      oMessage := 'No';
    end if;
  end index_ed;

  procedure index_info_add(iIndex_Id   qqb_hl_index_info.index_id%type,
                           iBegin_date qqb_hl_index_info.begin_date%type,
                           iEnd_date   qqb_hl_index_info.end_date%type,
                           iProcent    qqb_hl_index_info.procent%type,
                           oMessage    out varchar2) is
  begin
    insert into qqb_hl_index_info
      (id, index_id, begin_date, end_date, procent, created_on)
    values
      (qqb_hl_index_info_seq.nextval,
       iIndex_Id,
       iBegin_date,
       iEnd_date,
       iProcent,
       sysdate);
    oMessage := 'OK';
  exception
    when others then
      oMessage := 'No';
  end index_info_add;

  procedure index_info_ed(iId         qqb_hl_index_info.id%type,
                          iBegin_date qqb_hl_index_info.begin_date%type,
                          iEnd_date   qqb_hl_index_info.end_date%type,
                          iProcent    qqb_hl_index_info.procent%type,
                          oMessage    out varchar2) is
  begin
    update qqb_hl_index_info t
       set t.begin_date = iBegin_date,
           t.end_date   = iEnd_date,
           t.procent    = iProcent,
           t.created_on = sysdate
     where t.id = iId;
    oMessage := 'OK';
  exception
    when others then
      oMessage := 'No';
  end index_info_ed;

  procedure update_dep_percent_rate(iContract_id        number,
                                    iDate_validate_list array_varchar2,
                                    iPercent_rate_list  array_varchar2) is
    vPercent_type_list array_varchar2 := array_varchar2();
    vlibor_check       array_varchar2 := array_varchar2();
    v_ARRAY_ACCESS     array_varchar2;
  begin
    vPercent_type_list.Extend;
    vPercent_type_list(1) := 'DEP';
    vlibor_check.Extend;
    vlibor_check(1) := 'N';
    dep_api.Get_Access_BookMark(icontract_id  => iContract_id,
                                oarray_access => v_ARRAY_ACCESS);
    dep_api.contract_percent_action(iaction             => 'ENTER',
                                    icontract_id        => iContract_id,
                                    ipercent_type_list  => vPercent_type_list,
                                    idate_validate_list => iDate_validate_list,
                                    ipercent_rate_list  => iPercent_rate_list,
                                    i_libor_check       => vlibor_check);
  end update_dep_percent_rate;

  procedure percent_rate_synch(iIndex_id qqb_hl_indexes.id%type) is
    vContract_date dep_contracts.date_begin%type;
    vDate_validate array_varchar2 := array_varchar2();
    vPercent_rate  array_varchar2 := array_varchar2();
    v_percent      varchar2(20);
  
  begin
    for i in (select t.dep_id_b
                from qqb_hl_resurs t
               where t.dep_id_b = '377310'
                 and t.submanba_id in
                     (select s.id
                        from qqb_hl_spr_submanba s
                       where s.type = iIndex_id)) loop
    
      for rates in (select to_char(r.begin_date, 'dd.mm.yyyy') begin_date,
                           r.procent
                      from qqb_hl_index_info r
                     where r.index_id = iIndex_id
                     order by r.begin_date) loop
        vDate_validate.Extend;
        vDate_validate(vDate_validate.Last) := rates.begin_date;
        SELECT replace(decode(substr(to_char(rates.procent), 1, 1),
                              ',',
                              '0' || to_char(rates.procent),
                              to_char(rates.procent)),
                       ',',
                       '.')
          into v_percent
          FROM dual;
        vPercent_rate.Extend;
        vPercent_rate(vPercent_rate.Last) := v_percent;
        select c.contract_date
          into vContract_date
          from dep_contracts c
         where c.id = i.dep_id_b;
        --if (vDate_validate.Last < vContract_date) then
        update_dep_percent_rate(i.dep_id_b, vDate_validate, vPercent_rate);
        --        end if;
        vPercent_rate  := null;
        vDate_validate := null;
      end loop;
    end loop;
  
  end percent_rate_synch;

  --//субманбаага тегишли хисоб ракамларни бириктириш
  procedure detail_filial_ed(iId           qqb_hl_accounts.t2_id%type,
                             iFilial       qqb_hl_accounts.filial%type,
                             iAcc_res_bo   qqb_hl_accounts.acc_res_bo%type,
                             iAcc_foiz_bo  qqb_hl_accounts.acc_foiz_bo%type,
                             iAcc_res_fil  qqb_hl_accounts.acc_res_fil%type,
                             iAcc_foiz_fil qqb_hl_accounts.acc_foiz_fil%type,
                             oMessage      out varchar2) is
  begin
    if (iAcc_foiz_bo is null) then
      oMessage := 'Депозит Юр.Лиц. подсистемасида ' || iAcc_res_bo ||
                  ' ресурс хисоб ракамига фоиз хисоб раками бириктирилмаган!';
    elsif (iAcc_foiz_fil is null) then
      oMessage := 'Депозит Юр.Лиц. подсистемасида ' || iAcc_res_fil ||
                  ' ресурс хисоб ракамига фоиз хисоб раками бириктирилмаган!';
    else
      update qqb_hl_accounts r
         set r.acc_res_bo   = iAcc_res_bo,
             r.acc_foiz_bo  = iAcc_foiz_bo,
             r.acc_res_fil  = iAcc_res_fil,
             r.acc_foiz_fil = iAcc_foiz_fil
       where r.t2_id = iId
         and r.filial = iFilial;
      oMessage := 'OK';
    end if;
  end detail_filial_ed;
  --//confirm тасдиклаш
  procedure confirm(iId_res  in qqb_hl_resurs.id%type,
                    oMessage out varchar2) is
  begin
    vDate_operation := sysdate;
    select r.* into vResRow from qqb_hl_resurs r where r.id = iId_res;
    select g.*
      into vZvRow
      from glb_cred_zayav g
     where g.id = vResRow.Zv_Id;
    if vResRow.Summa != vZvRow.Zayav_Summa then
      oMessage := 'Ресурс суммаси кредит суммасига тенг эмас: ' ||
                  vResRow.Summa || ' != ' || vZvRow.Zayav_Summa;
    else
      update qqb_hl_resurs r set r.state = '3' where r.id = iId_res; --Статусни "Подтвержден ГО" га узгартириш
      prot(iId_res         => vResRow.Id,
           iIzox           => vResRow.Izox,
           iState_id       => '2',
           iOp_id          => null,
           iDate_operation => vDate_operation);
    end if;
  end confirm;

  function is_invalid_date(iId qqb_hl_index_info.id%type) return number is
    vMax_date  date;
    vOper_date date;
    vMaxId     qqb_hl_index_info.id%type;
  begin
    vOper_date := setup.get_operday;
    select t.* into vIdxInfRow from qqb_hl_index_info t where t.id = iId;
    select count(*)
      into vCount
      from qqb_hl_index_info t
     where t.index_id = vIdxInfRow.Index_Id;
    if vCount = 1 then
      select t.end_date
        into vMax_date
        from qqb_hl_index_info t
       where t.index_id = vIdxInfRow.Index_Id;
      if (vMax_date < vOper_date) then
        return 1;
      else
        return 0;
      end if;
    elsif vCount > 1 then
      select max(t.end_date)
        into vMax_date
        from qqb_hl_index_info t
       where t.index_id = vIdxInfRow.Index_Id;
      select t.id
        into vMaxId
        from qqb_hl_index_info t
       where t.index_id = vIdxInfRow.Index_Id
         and t.end_date = vMax_date;
      if (vMax_date < vOper_date and vMaxId = iId) then
        return 1;
      else
        return 0;
      end if;
    else
      return 0;
    end if;
  
  end is_invalid_date;

  --//хисоб ракам номини олиш
  function Get_Sch_Name(iSch accounts.acc_external%type) return varchar2 is
    result         j_array := j_array();
    vCondition     accounts.condition%type;
    vCode_currency accounts.code_currency%type;
    vCurrencyName  REF_CURRENCY.Name%type;
    vMfo           accounts.code_filial%type := '01037';
  begin
    vSchName := bank.Get_Account_Name_any(iSch, 'N', vMfo);
    select t.code_currency
      into vCode_currency
      from accounts t
     where t.code_filial = vMfo
       and t.acc_external = iSch;
    select v.Name
      into vCurrencyName
      from v_currency v
     where v.CODE = vCode_currency;
    if vSchName <> 'Нет сведений' then
      begin
        select 'X'
          into vCondition
          from accounts
         where code_filial = vMfo
           and acc_external = iSch
           and condition = 'A';
      exception
        when no_data_found then
          vSchName := 'Счет не открытый';
        when others then
          vSchName := 'Неизвестный счет коорспондента';
      end;
    end if;
    result.push(vSchName);
    result.push(vCode_currency);
    result.push(vCurrencyName);
    return result.to_string();
  end Get_Sch_Name;
  --//
  function get_proc_acc(iMfo accounts.code_filial%type,
                        iAcc accounts.acc_external%type) return varchar2 is
    result j_array := j_array();
  begin
    select substr(t.account_code, -20)
      into vAcc
      from dep_accounts t
     where t.account_type = '4'
       and t.date_next = '31.12.9999'
       and t.contract_id =
           (select t.contract_id
              from dep_accounts t
             where substr(t.account_code, -20) = iAcc
               and t.account_type = '1'
               and (select c.state
                      from dep_contracts c
                     where c.id = t.contract_id
                       and c.filial_code = iMfo) = 'APPROVE');
    result.push(vAcc);
    return result.to_string();
  end get_proc_acc;
  --//депозит шартномаси номини олиш
  function Get_Dep_Name(iDep_id dep_contracts.id%type) return varchar2 is
    result j_array := j_array();
    vName  dep_contracts.contract_name%type;
  begin
    select c.contract_name
      into vName
      from dep_contracts c
     where c.id = iDep_id;
    result.push(vName);
    return result.to_string();
  end Get_Dep_Name;
  --//депозит шартномаси номини олиш 2
  function Get_Dep_Name2(iDep_id dep_contracts.id%type) return varchar2 is
    vName dep_contracts.contract_name%type;
  begin
    select c.contract_name
      into vName
      from dep_contracts c
     where c.id = iDep_id;
    return vName;
  end Get_Dep_Name2;

  function Get_manba_Name(iZv_id glb_cred_zayav.id%type,
                          iType  number default 0) return varchar2 is
    vName qqb_hl_spr_manba.name%type;
  begin
    begin
      if iType = 0 then
        select d.manba
          into vName
          from glb_cred_zayav_desc d
         where d.id_zv = iZv_id;
      else
        select m.name
          into vName
          from qqb_hl_spr_manba m
         where m.code = (select d.manba
                           from glb_cred_zayav_desc d
                          where d.id_zv = iZv_id);
      end if;
    exception
      when others then
        vName := '';
    end;
    return vName;
  end Get_manba_Name;
  function Get_manba_Name1(iCode qqb_hl_spr_manba.code%type) return varchar2 is
    vName qqb_hl_spr_manba.name%type;
  begin
    begin
      select m.name
        into vName
        from qqb_hl_spr_submanba m
       where m.id = iCode;
    exception
      when others then
        vName := '';
    end;
    return vName;
  end Get_manba_Name1;

  function Get_manba_Name2(iCode qqb_hl_spr_manba.code%type) return varchar2 is
    result j_array := j_array();
    vName  qqb_hl_spr_manba.name%type;
  begin
    select m.name into vName from qqb_hl_spr_submanba m where m.id = iCode;
    result.push(vName);
    return result.to_string();
  end Get_manba_Name2;
  
 function Get_manba_Name3(iId qqb_hl_spr_submanba.id%type, iDate date)
   return varchar2 is
   result j_array := j_array();
   vProc  qqb_hl_index_info.procent%type;
 begin
   select nvl(t.procent, 0)
     into vProc
     from qqb_hl_index_info t
    where t.index_id =
          (select m.type from qqb_hl_spr_submanba m where m.id = iId)
      and t.begin_date <= iDate;
 exception
   when no_data_found then
     vProc := 0;
     result.push(vProc);
     return result.to_string();
 end Get_manba_Name3;

  procedure SendToMain(iId_res qqb_hl_resurs.id%type) is
  begin
  
    update qqb_hl_resurs a set a.state = '1' where a.id = iId_res;
    prot(iId_res, 'Отправлен в ГО', 1, 1, sysdate);
  
  end;

  -----/----------------/-------------------------/-------------------------------
  function chek_schedules_graph(iDep_contract_id dep_contracts.id%type)
    return number is
    vDep_id dep_contracts.id%type;
  begin
    begin
      select a.contract_id
        into vDep_id
        from dep_schedules_forecast a
       where a.contract_id = iDep_contract_id
         and rownum = 1;
      if sql%found then
        return null;
      end if;
    exception
      when no_data_found then
        return 1;
    end;
  end;

  procedure generator_graph(iLoan_id                ln_graph_debt.loan_id%type,
                            iDep_contract_id        dep_contracts.id%type,
                            IDep_filial_contract_id dep_contracts.id%type) is
  
  begin
    ---bosh bank uchun grafik geranatsiya
    if chek_schedules_graph(iDep_contract_id) is not null then
      for rr in (select * from ln_graph_debt g where g.loan_id = iLoan_id) loop
        insert into dep_schedules_forecast
          (id,
           contract_id,
           type,
           date_validate,
           amount,
           emp_code,
           date_modify)
        values
          (dep_schedules_seq.nextval,
           iDep_contract_id,
           1,
           rr.date_red,
           rr.summ_red,
           setup.get_employee_code,
           sysdate);
      
      end loop;
    else
      Raise_Application_Error(-20000,
                              iDep_contract_id ||
                              ' Contract id графиги киритилган');
    end if;
    ---filial uchun grafik geranatsiya
    if chek_schedules_graph(IDep_filial_contract_id) is not null then
      for rr in (select * from ln_graph_debt g where g.loan_id = iLoan_id) loop
        insert into dep_schedules_forecast
          (id,
           contract_id,
           type,
           date_validate,
           amount,
           emp_code,
           date_modify)
        values
          (dep_schedules_seq.nextval,
           IDep_filial_contract_id,
           1,
           rr.date_red,
           rr.summ_red,
           setup.get_employee_code,
           sysdate);
      
      end loop;
    else
      Raise_Application_Error(-20000,
                              IDep_filial_contract_id ||
                              ' Contract id графиги киритилган');
    end if;
  
  end;
  --------------------------------------
  ----------------Azizbek---------------
  --------------------------------------
  Procedure contract_action(i_action_resource       dep_s_action_resource.code%type,
                            i_client_code           client_current.code%type,
                            i_contract_name         dep_contracts.contract_name%type,
                            i_contract_number       dep_contracts.contract_number%type,
                            i_account_layouts       dep_account_layouts.id%type,
                            i_deposit_type          dep_s_deposit_type.code%type,
                            i_contract_date         dep_contracts.contract_date%type,
                            i_date_begin            dep_contracts.date_begin%type,
                            i_date_end              dep_contracts.date_end%type,
                            i_summa                 dep_contracts.summa%type,
                            i_count_days            dep_contracts.count_days%type,
                            i_currency_code         dep_contracts.currency_code%type,
                            i_desc_days_in_year     DEP_s_DAYS_IN_YEAR.Code%type,
                            i_percent               DEP_CONTRACTS_PERCENT_RATE.PERCENT_RATE%type,
                            i_percent_DATE_VALIDATE varchar2,
                            i_Libor_check           varchar2,
                            o_oid                   out number,
                            o_error                 out varchar2) is
    v_ARRAY_ACCESS      ARRAY_VARCHAR2;
    vtypeCode           varchar2(255);
    vstate              varchar2(255);
    vPERCENT_TYPE_LIST  ARRAY_VARCHAR2 := array_varchar2();
    vDATE_VALIDATE_LIST ARRAY_VARCHAR2 := array_varchar2();
    vPERCENT_RATE_LIST  ARRAY_VARCHAR2 := array_varchar2();
    vLIBOR_CHECK        ARRAY_VARCHAR2 := array_varchar2();
    v_percent           varchar2(20);
  begin
    SELECT decode(substr(to_char(i_percent), 1, 1),
                  '.',
                  '0' || to_char(i_percent),
                  to_char(i_percent))
      into v_percent
      FROM dual;
  
    dep_api.contract_action(iaction                      => 'ENTER',
                            iid                          => null,
                            oid                          => o_oid,
                            iaccount_layouts             => i_account_layouts,
                            iaction_resource             => i_action_resource,
                            icurrency_type               => 'FOREIGN',
                            iclient_type                 => 'FILIAL',
                            ideposit_type                => i_deposit_type,
                            iclient_code                 => i_client_code,
                            icontract_name               => i_contract_name,
                            icontract_number             => i_contract_number,
                            icontract_date               => i_contract_date,
                            idate_begin                  => i_date_begin,
                            idate_end                    => i_date_end,
                            idate_end_fact               => i_date_end,
                            icurrency_code               => i_currency_code,
                            isumma                       => i_summa,
                            icount_days                  => i_count_days,
                            imin_summa_calc_percent      => 0,
                            ipercent_min_summa           => 0,
                            idesc_days_in_year           => i_desc_days_in_year,
                            idesc_capitalization_code    => 0,
                            idesc_calc_percent_type      => 'SALDO',
                            idesc_procent_last           => 0,
                            idesc_take_tax               => 'Y',
                            iswift_code_bank_a           => null,
                            iswift_code_bank_b           => null,
                            iswift_from_bank_a           => null,
                            iswift_from_bank_b           => null,
                            iswift_to_bank_a             => null,
                            iswift_to_bank_b             => null,
                            iswift_proc_from_account     => null,
                            iswift_proc_to_account       => null,
                            iswift_related_reference     => null,
                            iswift_receiver_information  => null,
                            i_parent_contract_id         => null,
                            i_pledge                     => 'Y',
                            i_product_select             => null,
                            i_min_balance_operation      => 0,
                            i_credit_source_code         => null,
                            i_foreign_organization_code  => null,
                            i_financing_currency_code    => null,
                            i_financing_amount           => null,
                            i_issuer                     => null,
                            i_denomination               => null,
                            i_release_series             => null,
                            i_interest_frequenc          => null,
                            i_mm_id                      => null,
                            i_remark_line0               => null,
                            i_remark_line1               => null,
                            i_bloomberg_reuters          => null,
                            iswift_from_bank_acc         => null,
                            iswift_to_bank_acc           => null,
                            iswift_56_a                  => null,
                            iswift_56_acc                => null,
                            iswift_to_bank_b_acc         => null,
                            iswift_receiver_inf72        => null,
                            iswift_credit_or_debit_c     => null,
                            iswift_credit_or_debit_acc_c => null,
                            IEMP_RESOURCE_CODE           => null,
                            IBUDGET_ACC_OSN              => null,
                            IBUDGET_ACC_PERC             => null,
                            IIS_ANNUITET                 => null,
                            iuser_id                     => null);
    dep_api.Get_Access_BookMark(icontract_id  => o_oid,
                                oarray_access => v_ARRAY_ACCESS);
  
    vPERCENT_TYPE_LIST.Extend;
    vPERCENT_TYPE_LIST(1) := 'DEP';
    vdate_validate_list.Extend;
    vdate_validate_list(1) := i_percent_DATE_VALIDATE;
    vPERCENT_RATE_LIST.Extend;
    vPERCENT_RATE_LIST(1) := v_percent;
    vlibor_check.Extend;
    vlibor_check(1) := i_Libor_check;
  
    dep_api.contract_percent_action(iaction             => 'ENTER',
                                    icontract_id        => o_oid,
                                    ipercent_type_list  => vPERCENT_TYPE_LIST,
                                    idate_validate_list => vdate_validate_list,
                                    ipercent_rate_list  => vpercent_rate_list,
                                    i_libor_check       => vlibor_check);
  
    if upper(v_ARRAY_ACCESS(1)) = 'Y' then
      SELECT dep_api.get_obj_contract into vtypeCode FROM dual;
      dep_api.initialize_object(iObject_Id        => o_oid,
                                iObject_Type_Code => vtypeCode);
      SELECT dep_api.get_contract_param(ikey => 'STATE')
        into vstate
        FROM dual;
    end if;
  exception
    when others then
      o_error := sqlerrm;
  end contract_action;
  ----Утвердить: Dep_jur
  PROCEDURE contract_action_approve(IID_LIST IN ARRAY_VARCHAR2,
                                    OMESSAGE OUT VARCHAR2) IS
  BEGIN
    dep_api.contract_action(OMESSAGE => OMESSAGE,
                            IACTION  => 'APPROVE',
                            IID_LIST => IID_LIST);
  END contract_action_approve;
  
  Procedure accept(i_submanba_id           number,
                   i_contract_name         dep_contracts.contract_name%type,
                   i_contract_number       dep_contracts.contract_number%type,
                   i_contract_date         varchar2,
                   i_date_begin            varchar2,
                   i_date_end              varchar2,
                   i_percent               DEP_CONTRACTS_PERCENT_RATE.PERCENT_RATE%type,
                   i_percent_DATE_VALIDATE varchar2,
                   i_desc_days_in_year     DEP_s_DAYS_IN_YEAR.Code%type,
                   i_zv_id                 number,
                   i_resurs_id             qqb_hl_resurs.id%type,
                   o_Message               out varchar2) is
  
    v_dep_id_b          integer;
    v_dep_id_f          integer;
    v_error             varchar2(32767);
    v_action_resource   dep_s_action_resource.code%type;
    v_account_layouts   dep_account_layouts.id%type; --Макеты счетов; Межфил. депозит 16102-840
    v_deposit_type      dep_s_deposit_type.code%type; --Справочник типов вкладов; Межфилиальный депозит
    v_contract_date     dep_contracts.contract_date%type := to_date(i_contract_date,
                                                                    'dd.mm.yyyy');
    v_date_end          dep_contracts.date_end%type;
    v_currency_code     dep_contracts.currency_code%type;
  
    v_summa             dep_contracts.summa%type;
    v_cl_code           dep_contracts.client_code%type;
    v_loan_id           ln_card.loan_id%type;
    v_count_days        dep_contracts.count_days%type;
    v_date_begin        ln_card.open_date%type;
  
    vEmpCode            core_users.user_id%type; --current
    vEmpHeaderCode      core_users.user_id%type; --bosh bank
    vEmpZvCode          core_users.user_id%type; --zayavka
    v_mfo               char(5);
    v_currency          qqb_hl_spr_submanba.currency%type;
    v_Libor_check       varchar2(1):='N';
-----------------------------------------------------------
    boshMfo             varchar2(5) := '01037';
    v_acc_res_bo        accounts.acc_external%type;
    v_acc_res_fil       accounts.acc_external%type;
    v_acc_code          accounts.code%type;
    f_acc_code          accounts.code%type;
    
begin
 begin
     select t.currency
       into v_currency
       from qqb_hl_spr_submanba t
      where id = i_submanba_id;
     select t.cl_code, t.loan_id, t.mfo
       into v_cl_code, v_loan_id, v_mfo
       from glb_cred_zayav t
      where id = i_zv_id;
-----------------------------------------------------------
  select t.acc_res_bo, t.acc_res_fil
    into v_acc_res_bo, v_acc_res_fil
    from QQB_HL_ACCOUNTS t
   where t.filial = v_mfo
     and t.t2_id  = i_submanba_id;
  select a.code
    into v_acc_code
    from accounts a
   where a.code_filial = boshMfo
     and a.acc_external = v_acc_res_bo;
  select a.code
    into f_acc_code
    from accounts a
   where a.code_filial = v_mfo
     and a.acc_external = v_acc_res_fil;
------------------------------------------------------
  select da.contract_id
    into v_dep_id_b
    from dep_accounts da
   where da.account_code = v_acc_code
     and exists (select *
                   from dep_contracts a
                  where a.id = da.contract_id
                    and a.state = 'APPROVE')
     and rownum = 1;
  select da.contract_id
    into v_dep_id_f
    from dep_accounts da
   where da.account_code = f_acc_code
     and exists (select *
                   from dep_contracts a
                  where a.id = da.contract_id
                    and a.state = 'APPROVE')
     and rownum = 1;

  if sql%found then
    update qqb_hl_resurs t
       set t.submanba_id = i_submanba_id,
           t.dep_id_b    = v_dep_id_b,
           t.dep_id_f    = v_dep_id_f,
           t.state       = 2
     where t.id = i_resurs_id;
  end if;
 exception when no_data_found then  
   begin
      if v_cl_code is null then
        o_Message := 'Кредит бюртмасига мижоз уникал коди киритилмаган?!';
        Raise_Application_Error(-20000, o_Message);
      end if;
      if v_loan_id is null then
        o_Message := 'Кредит бюртмасига loan_id бириктирилмаган?!';
        Raise_Application_Error(-20000, o_Message);
      end if;
      vEmpCode := setup.get_employee_code();
      select a.user_id
        into vEmpHeaderCode
        from core_users a
       where a.filial_code = '01037'
         and a.private_post_id = 4
         and a.state = 'A';
      select a.user_id
        into vEmpZvCode
        from core_users a
       where a.filial_code = v_mfo
         and a.private_post_id = 4
         and a.state = 'A';
    
      if v_loan_id is not null then
        select nvl(t.summ_loan, 0) / 100,
               t.currency,
               t.open_date,
               t.close_date
          into v_summa,
               v_currency_code,
               v_date_begin,
               v_date_end
          from ln_card t
         where loan_id = v_loan_id;
        if i_date_begin is not null then
          v_date_begin := to_date(i_date_begin, 'dd.mm.yyyy');
        end if;
        if i_date_end is not null then
          v_date_end := to_date(i_date_end, 'dd.mm.yyyy');
        end if;
        SELECT v_date_end - v_date_begin into v_count_days FROM dual;
        -- Кредит Libor маржаси кушиб узгартирилади
        -- Кредит Libor маржаси буйича хисоблашини аниклаш керак
        /*        fozil_actions.ln_percent_update(iLoanId     => v_loan_id,
                                                iTypeAction => 'NEW' );
        */
      
        ----Shartnoma; Размещение
        setup.set_employee_code(vEmpHeaderCode);
        v_action_resource := 'PLACEMENT';
        if v_currency = '978' then
          v_account_layouts := 74; --Макеты счетов; Межфил. депозит 16102-978
        end if;
        if v_currency = '840' then
          v_account_layouts := 63; --Макеты счетов; Межфил. депозит 16102-840
        end if;
      
        v_deposit_type := 4; --Справочник типов вкладов; Межфилиальный депозит
        contract_action(i_action_resource       => v_action_resource,
                        i_client_code           => lpad(v_mfo, 8, '0'),
                        i_contract_name         => i_contract_name,
                        i_contract_number       => i_contract_number,
                        i_account_layouts       => v_account_layouts,
                        i_deposit_type          => v_deposit_type,
                        i_contract_date         => v_contract_date,
                        i_date_begin            => v_date_begin,
                        i_date_end              => v_date_end,
                        i_summa                 => v_summa,
                        i_count_days            => v_count_days,
                        i_currency_code         => v_currency_code,
                        i_desc_days_in_year     => i_desc_days_in_year,
                        i_percent               => i_percent,
                        i_percent_DATE_VALIDATE => i_percent_DATE_VALIDATE,
                        i_Libor_check           => v_Libor_check,
                        o_oid                   => v_dep_id_b,
                        o_error                 => v_error);
        if v_error is not null then
          rollback;
          o_Message := v_error;
          Raise_Application_Error(-20000, v_error);
        else
          ----Shartnoma; Привлечение
          setup.set_employee_code(vEmpZvCode);
          v_action_resource := 'ATTRACT';
          if v_currency = '978' then
            v_account_layouts := 76; --Макеты счетов; Межфил. депозит 16102-978
          end if;
          if v_currency = '840' then
            v_account_layouts := 62; --Макеты счетов; Межфил. депозит 16102-840
          end if;
          v_deposit_type := 4; --Справочник типов вкладов; Межфилиальный депозит
          contract_action(i_action_resource       => v_action_resource,
                          i_client_code           => '00001037',
                          i_contract_name         => i_contract_name,
                          i_contract_number       => i_contract_number,
                          i_account_layouts       => v_account_layouts,
                          i_deposit_type          => v_deposit_type,
                          i_contract_date         => v_contract_date,
                          i_date_begin            => v_date_begin,
                          i_date_end              => v_date_end,
                          i_summa                 => v_summa,
                          i_count_days            => v_count_days,
                          i_currency_code         => v_currency_code,
                          i_desc_days_in_year     => i_desc_days_in_year,
                          i_percent               => i_percent,
                          i_percent_DATE_VALIDATE => i_percent_DATE_VALIDATE,
                          i_Libor_check           => v_Libor_check,
                          o_oid                   => v_dep_id_f,
                          o_error                 => v_error);
        
          if v_error is not null then
            o_Message := v_error;
            rollback;
            Raise_Application_Error(-20000, v_error);
          else
            ---Shaxzod qo'shgan grafik genratsiya qiladi
            generator_graph(iLoan_id                => v_Loan_id,
                            iDep_contract_id        => v_dep_id_b,
                            IDep_filial_contract_id => v_dep_id_f);
          
            ---Деп Юр Лиц бириктириш
            update qqb_hl_resurs t
               set t.submanba_id = i_submanba_id,
                   t.dep_id_b    = v_dep_id_b,
                   t.dep_id_f    = v_dep_id_f,
                   t.state       = 2
             where t.id = i_resurs_id;
            --Nazarov X.A 28.09.2020
            --- Настройка режим работ
            --Головной офис
            insert into dep_contracts_mode_actions
              (contract_id,
               mode_calc_percent,
               mode_redemp_percent,
               deposit_delinquency_control,
               percent_delinquency_control,
               emp_code,
               date_modify,
               auto_close_contract,
               interest_methods,
               value_date)
            values
              (v_dep_id_b,
               'ACD',
               'MANUAL',
               'N',
               'N',
               vEmpHeaderCode,
               sysdate,
               'Y',
               'USUAL',
               'N');
            insert into dep_contracts_mode_actions_his
              (contract_id,
               mode_calc_percent,
               mode_redemp_percent,
               deposit_delinquency_control,
               percent_delinquency_control,
               emp_code,
               date_modify,
               auto_close_contract,
               interest_methods,
               value_date)
            values
              (v_dep_id_b,
               'ACD',
               'MANUAL',
               'N',
               'N',
               vEmpHeaderCode,
               sysdate,
               'Y',
               'USUAL',
               'N');
            --филиал
            insert into dep_contracts_mode_actions
              (contract_id,
               mode_calc_percent,
               mode_redemp_percent,
               deposit_delinquency_control,
               percent_delinquency_control,
               emp_code,
               date_modify,
               auto_close_contract,
               interest_methods,
               value_date)
            values
              (v_dep_id_f,
               'ACD',
               'MANUAL',
               'N',
               'N',
               vEmpZvCode,
               sysdate,
               'Y',
               'USUAL',
               'N');
            insert into dep_contracts_mode_actions_his
              (contract_id,
               mode_calc_percent,
               mode_redemp_percent,
               deposit_delinquency_control,
               percent_delinquency_control,
               emp_code,
               date_modify,
               auto_close_contract,
               interest_methods,
               value_date)
            values
              (v_dep_id_f,
               'ACD',
               'MANUAL',
               'N',
               'N',
               vEmpZvCode,
               sysdate,
               'Y',
               'USUAL',
               'N');
            --Счетлар бириктириш. йук булса очиб бириктириш
            openBindAcc(v_mfo,
                        i_submanba_id,
                        v_dep_id_b,
                        v_dep_id_f,
                        o_Message);
            -- Sherzodaka yozgan protokolni chaqirilgan
            qqb_resurs_val.prot(i_resurs_id,
                                'Рухсат берилди',
                                2,
                                3,
                                sysdate);
            --o_Message := 'Ma`lumotlar muvaffaqiyatli saqlandi';
          end if;
        end if;
      else
        o_Message := 'Мижоз кредит системасига киритилмаган!!!';
        Raise_Application_Error(-20000,
                                'Мижоз кредит системасига киритилмаган!!!');
      end if;
      setup.set_employee_code(vEmpCode);
   exception
   when others then
     setup.set_employee_code(vEmpCode);
     o_Message := sqlerrm;
     Raise_Application_Error(-20000, sqlerrm);
   end;
 end;
end accept;

  procedure bindAccount(iAccount_code  varchar2,
                        iFilialCode    varchar2,
                        iContractId    varchar2,
                        iResultMessage out varchar2) is
    vAccountTypeCode array_varchar2;
    vFilialCode      array_varchar2;
    vAccount_code    array_varchar2;
    vSubCoa          array_varchar2;
    vDateValidate    array_varchar2;
  begin
    vAccount_code := array_varchar2();
    vAccount_code.extend();
    vAccount_code(1) := iAccount_code;
  
    vFilialCode := array_varchar2();
    vFilialCode.extend();
    vFilialCode(1) := iFilialCode;
  
    vAccountTypeCode := array_varchar2();
    vAccountTypeCode.extend();
    vAccountTypeCode(1) := '1';
  
    vSubCoa := array_varchar2();
    vSubCoa.extend();
    vSubCoa(1) := '1';
  
    vDateValidate := array_varchar2();
    vDateValidate.extend();
    vDateValidate(1) := to_char(setup.get_operday, 'dd.mm.yyyy');
    Dep_Api.account_action(icontract_id       => iContractId,
                           iaction            => 'ACCOUNT_CREATE',
                           iaccount_code      => vAccount_code,
                           iaccount_type_code => vAccountTypeCode,
                           idate_validate     => vDateValidate,
                           ifilial_code       => vFilialCode,
                           isub_coa           => vSubCoa,
                           ipreview_mode      => 'ON',
                           oresult_message    => iResultMessage);
  end;
  ----- Bitta hisob raqam ochish
  procedure createAccount(iCreatorFilial varchar2,
                          iBindFilial    varchar2,
                          iTemplate      varchar2,
                          iClentName     varchar2,
                          iOrdering      varchar2 default '310',
                          oCondition     out varchar2,
                          oDialog        out varchar2,
                          oMessage       out varchar2) is
    oAccountCode   ACCOUNTS.CODE%TYPE;
    vEmpCode       core_users.user_id%type;
    iOperation     varchar2(1) := 'A';
    vFilialEmpCode core_users.user_id%type;
    oBindMessage   varchar2(5000);
    vClUId         client_current.client_uid%type;
  
  begin
    BEGIN
      vEmpCode := setup.get_employee_code;
      select a.user_id
        into vFilialEmpCode
        from core_users a
       where a.filial_code = iCreatorFilial
         and a.private_post_id = 4
         and a.state = 'A';
      --vFilialEmpCode:=setup.get_employee_code;
      setup.set_employee_code(vFilialEmpCode);
      oAccountCode := '11' || setup.get_local_code || iTemplate || '000' ||
                      iBindFilial || iOrdering;
      select substr(oAccountCode, 17, 8) into vClUId from dual;
      account.action(vClUId,
                     oAccountCode,
                     iCreatorFilial,
                     iClentName,
                     'A',
                     'B',
                     setup.get_operday,
                     iOperation,
                     oCondition,
                     oDialog,
                     oMessage);
      -- dbms_output.put_line('account.action -> ' || oAccountCode||','||iCreatorFilial||','||iClentName||','||'A,B,'||''''||setup.get_operday||''''||'iOperation,oCondition,oDialog,oMessage');
      -- dbms_output.put_line(oMessage);
      bindAccount(oAccountCode,
                  iCreatorFilial,
                  dep_api.get_contract_param('ID'),
                  oBindMessage);
      -- dbms_output.put_line(oBindMessage);
      setup.set_employee_code(vEmpCode);
    EXCEPTION
      WHEN others THEN
        setup.set_employee_code(vEmpCode);
        oMessage := SQLERRM;
    END;
  end;

  procedure createAccounts(iT2_ID      varchar2,
                           iFilial     varchar2,
                           iClientName varchar2,
                           oCondition  out varchar2,
                           oDialog     out varchar2,
                           oMessage    out varchar2) is
    vRow       qqb_hl_accounts%rowtype;
    vTemplate  varchar2(30);
    vCondition varchar2(40) := '';
    vDialog    varchar2(32000) := '';
    vMessage   varchar2(32000) := '';
    vOrdering  varchar2(3) := '310';
  
    vBO_res_schet   varchar2(30);
    vFIL_res_schet  varchar2(30);
    vBO_foiz_schet  varchar2(30);
    vFIL_foiz_schet varchar2(30);
  begin
    for vRow in (select *
                   from qqb_hl_accounts acc
                  where acc.filial = iFilial
                    and acc.t2_id = iT2_ID) loop
      vTemplate := substr(vRow.Acc_Res_Bo, 1, 9);
      if substr(vRow.Acc_Res_Bo, 9, 1) = '_' then
        -- bek ofis uchun 16102 resurs HR
        createAccount('01037',
                      vRow.Filial,
                      vTemplate,
                      iClientName,
                      vOrdering,
                      vCondition,
                      vDialog,
                      vMessage);
        oCondition    := vCondition;
        oDialog       := vDialog;
        oMessage      := vMessage;
        vBO_res_schet := vMessage;
      end if;
    
      vTemplate := substr(vRow.Acc_Foiz_Bo, 1, 9);
      if substr(vTemplate, 9, 1) = '_' then
        -- bek ofis uchun 16304 foiz HR
        createAccount('01037',
                      vRow.Filial,
                      vTemplate,
                      iClientName,
                      vOrdering,
                      vCondition,
                      vDialog,
                      vMessage);
        oCondition     := oCondition || '#' || vCondition;
        oDialog        := oDialog || '#' || vDialog;
        oMessage       := oMessage || '#' || vMessage;
        vBO_foiz_schet := vMessage;
      end if;
    
      vTemplate := substr(vRow.Acc_Res_Fil, 1, 9);
      if substr(vTemplate, 9, 1) = '_' then
        -- filial uchun 22203 resurs HR
        createAccount(vRow.Filial,
                      '01037',
                      vTemplate,
                      iClientName,
                      vOrdering,
                      vCondition,
                      vDialog,
                      vMessage);
        oCondition     := vCondition;
        oDialog        := vDialog;
        oMessage       := vMessage;
        vFIL_res_schet := vMessage;
      end if;
    
      vTemplate := substr(vRow.Acc_Foiz_Fil, 1, 9);
      if substr(vTemplate, 9, 1) = '_' then
        -- filial uchun 22409 foiz HR
        createAccount(vRow.Filial,
                      '01037',
                      vTemplate,
                      iClientName,
                      vOrdering,
                      vCondition,
                      vDialog,
                      vMessage);
        oCondition      := oCondition || '#' || vCondition;
        oDialog         := oDialog || '#' || vDialog;
        oMessage        := oMessage || '#' || vMessage;
        vFIL_foiz_schet := vMessage;
      end if;
    
      update qqb_hl_accounts t
         set t.acc_res_bo   = substr(vBO_res_schet, 8),
             t.acc_foiz_bo  = substr(vBO_foiz_schet, 8),
             t.acc_res_fil  = substr(vfil_res_schet, 8),
             t.acc_foiz_fil = substr(vfil_foiz_schet, 8)
       where t.t2_id = iT2_ID
         and t.filial = iFilial
         and length(vBO_res_schet) = 27
         and length(vBO_foiz_schet) = 27
         and length(vfil_res_schet) = 27
         and length(vfil_foiz_schet) = 27;
    end loop;
  end;
  --Nazarox X.A  28.09.2020 h/r biriktirish
  procedure openBindAcc(iMfo        varchar2,
                        iSubmanbaId integer,
                        b_id        dep_contracts.id%type,
                        f_id        dep_contracts.id%type,
                        oMessage    out varchar2) is
    v_acc_res_bo    accounts.acc_external%type;
    v_acc_foiz_bo   accounts.acc_external%type;
    v_acc_res_fil   accounts.acc_external%type;
    v_acc_foiz_fil  accounts.acc_external%type;
    v_acc_code      accounts.code%type;
    f_acc_code      accounts.code%type;
    v_acc_external  accounts.acc_external%type;
    v_acc_condition accounts.condition%type;
    vCode_b         v_emp_pass.code%type;
    vCode_f         v_emp_pass.code%type;
    iCondition      client_current.condition%type;
    vClUId          client_current.client_uid%type;
    filial_unqcode  accounts.client_code%type;
    bosh_unqcode    accounts.client_code%type := '00001037';
    account_name    accounts.name%type;
    tartibNo        char(3);
    oAccountCode    ACCOUNTS.CODE%TYPE;
    iOperation      S_ACC_OPERATION.OPERATION%TYPE;
    oCondition      S_ACC_OPERATION.CONDITION_RESULT%TYPE;
    oDialog         S_ACC_OPERATION.DIALOG_NEED%TYPE;
    boshMfo         varchar2(5) := '01037';
    v_id            accounts.id%type;
    v_code_coa      accounts.code_coa%type;
    v_foiz_code     accounts.code%type;
    f_foiz_code     accounts.code%type;
    --типы счета
    vCurrCode       qqb_hl_spr_submanba.currency%type;
    vAccId_6        accounts.id%type;
    vAccId_11       accounts.id%type;
    vAccId_12       accounts.id%type;
    f_acc_code_5    accounts.code%type;
    f_acc_code_11   accounts.code%type;
    f_acc_code_12   accounts.code%type;
  begin
    select p.code
      into vCode_b
      from v_emp_pass p
     where p.filial_code = boshMfo
       and p.rank_code = 4
       and p.condition = 'A';
    select p.code
      into vCode_f
      from v_emp_pass p
     where p.filial_code = iMfo
       and p.rank_code = 4
       and p.condition = 'A';
    filial_unqcode := lpad(iMfo, 8, '0');
    begin
      select t.acc_res_bo, t.acc_foiz_bo, t.acc_res_fil, t.acc_foiz_fil
        into v_acc_res_bo, v_acc_foiz_bo, v_acc_res_fil, v_acc_foiz_fil
        from QQB_HL_ACCOUNTS t
       where t.filial = iMfo
         and t.t2_id = iSubmanbaId;
      if sql%found then
        select c.condition
          into iCondition
          from client_current c
         where c.code = filial_unqcode;
        if iCondition = 'T' then
          update client_current t
             set t.condition             = 'A',
                 t.date_validate         = sysdate,
                 t.date_change_condition = setup.get_operday
           where t.code = filial_unqcode;
          insert into client_history
            select * from client_current where code = filial_unqcode;
        end if;
        select c.condition
          into iCondition
          from client_current c
         where c.code = bosh_unqcode;
        if iCondition = 'T' then
          update client_current t
             set t.condition             = 'A',
                 t.date_validate         = sysdate,
                 t.date_change_condition = setup.get_operday
           where t.code = bosh_unqcode;
          insert into client_history
            select * from client_current where code = bosh_unqcode;
        end if;
      
      end if;
    exception
      when no_data_found then
        oMessage := 'QQB_HL_ACCOUNTS таблицада шаблон топилмади?!';
        return;
    end;
    --Bosh bankda asosiy schet(16102)
    begin
      select a.code, a.condition, a.id, a.code_coa
        into v_acc_code, v_acc_condition, v_id, v_code_coa
        from accounts a
       where a.code_filial = boshMfo
         and a.acc_external = v_acc_res_bo;
    exception
      when no_data_found then
        --01037 da 16102 ochish;
        setup.Set_Employee_Code(vCode_b);
        --tartib nomer
        if substr(v_acc_res_bo, -3) = '___' then
          begin
            select lpad(max(substr(a.acc_external, -3)) + 1, 3, '0')
              into tartibNo
              from accounts a
             where a.code_filial = boshMfo
               and a.acc_external like
                   substr(v_acc_res_bo, 1, 9) || filial_unqcode || '___';
          exception
            when no_data_found then
              tartibNo := '001';
          end;
        else
          tartibNo := substr(v_acc_res_bo, -3);
        end if;
        --schet nomi
        select t.name || ' кредит линияси буйича ' || b.name ||
               'га берилган ресурслар'
          into account_name
          from QQB_HL_SPR_SUBMANBA t, bank_desc_glb b
         where t.id = iSubmanbaId
           and b.code = iMfo;
        --
        oAccountCode := '11' || setup.get_local_code ||
                        substr(v_acc_res_bo, 1, 9) || filial_unqcode ||
                        tartibNo;
        begin
          select a.code
            into v_acc_code
            from accounts a
           where a.code like oAccountCode;
        exception
          when no_data_found then
            select c.client_uid
              into vClUId
              from client_current c
             where c.code = filial_unqcode
               and rownum = 1;
            iOperation := 'A';
            account.action(vClUId,
                           oAccountCode,
                           boshMfo,
                           account_name,
                           'A',
                           'B',
                           setup.get_operday,
                           iOperation,
                           oCondition,
                           oDialog,
                           oMessage);
            v_acc_code := oAccountCode;
          
        end;
    end;
    --ДЮЛ ва qqb_hl_accountsга Бириктириш
    begin
      select a.code, a.acc_external, a.id, a.code_coa
        into v_acc_code, v_acc_external, v_id, v_code_coa
        from accounts a
       where a.code = v_acc_code;
      if sql%found then
        update dep_accounts
           set date_next = setup.get_operday
         where contract_id = b_id
           and account_type = 1
           and date_next = to_date('31.12.9999', 'dd.mm.yyyy');
        insert into dep_accounts
          (contract_id,
           account_type,
           date_validate,
           filial_code,
           account_code,
           emp_code,
           date_modify,
           date_next,
           acc_id,
           coa)
        values
          (b_id,
           1,
           setup.get_operday,
           boshMfo,
           v_acc_code,
           vCode_b,
           sysdate,
           to_date('31.12.9999', 'dd.mm.yyyy'),
           v_id,
           v_code_coa);
        insert into dep_accounts_his
          (contract_id,
           account_type,
           date_validate,
           filial_code,
           account_code,
           emp_code,
           date_modify,
           action)
        values
          (b_id,
           1,
           setup.get_operday,
           boshMfo,
           v_acc_code,
           vCode_b,
           sysdate,
           'I');
        update qqb_hl_accounts h
           set h.acc_res_bo = v_acc_external
         where h.t2_id = iSubmanbaId
           and h.filial = iMfo;
      end if;
    exception
      when others then
        null;
    end;
    --Bosh bankda procent schet(16304)
    begin
      select a.code, a.condition, a.id, a.code_coa
        into v_foiz_code, v_acc_condition, v_id, v_code_coa
        from accounts a
       where a.code_filial = boshMfo
         and a.acc_external = v_acc_foiz_bo;
    exception
      when no_data_found then
        --01037 da 16304 ochish;
        setup.Set_Employee_Code(vCode_b);
        --tartib nomer
        if substr(v_acc_foiz_bo, -3) = '___' then
          begin
            select lpad(max(substr(a.acc_external, -3)) + 1, 3, '0')
              into tartibNo
              from accounts a
             where a.code_filial = boshMfo
               and a.acc_external like
                   substr(v_acc_foiz_bo, 1, 9) || filial_unqcode || '___';
          exception
            when no_data_found then
              tartibNo := '001';
          end;
        else
          tartibNo := substr(v_acc_foiz_bo, -3);
        end if;
        --schet nomi
        select t.name || ' кредит линияси буйича ' || b.name ||
               'га берилган ресурсга хисоб.фоизлар'
          into account_name
          from QQB_HL_SPR_SUBMANBA t, bank_desc_glb b
         where t.id = iSubmanbaId
           and b.code = iMfo;
        --
        oAccountCode := '11' || setup.get_local_code ||
                        substr(v_acc_foiz_bo, 1, 9) || filial_unqcode ||
                        tartibNo;
        begin
          select a.code
            into v_foiz_code
            from accounts a
           where a.code like oAccountCode;
        exception
          when no_data_found then
            select c.client_uid
              into vClUId
              from client_current c
             where c.code = filial_unqcode
               and rownum = 1;
            iOperation := 'A';
            account.action(vClUId,
                           oAccountCode,
                           boshMfo,
                           account_name,
                           'A',
                           'B',
                           setup.get_operday,
                           iOperation,
                           oCondition,
                           oDialog,
                           oMessage);
            v_foiz_code := oAccountCode;
          
        end;
    end;
    --ДЮЛ ва qqb_hl_accountsга Бириктириш
    begin
      select a.code, a.acc_external, a.id, a.code_coa
        into v_foiz_code, v_acc_external, v_id, v_code_coa
        from accounts a
       where a.code = v_foiz_code;
      if sql%found then
        update dep_accounts
           set date_next = setup.get_operday
         where contract_id = b_id
           and account_type = 4
           and date_next = to_date('31.12.9999', 'dd.mm.yyyy');
        insert into dep_accounts
          (contract_id,
           account_type,
           date_validate,
           filial_code,
           account_code,
           emp_code,
           date_modify,
           date_next,
           acc_id,
           coa)
        values
          (b_id,
           4,
           setup.get_operday,
           boshMfo,
           v_foiz_code,
           vCode_b,
           sysdate,
           to_date('31.12.9999', 'dd.mm.yyyy'),
           v_id,
           v_code_coa);
        insert into dep_accounts_his
          (contract_id,
           account_type,
           date_validate,
           filial_code,
           account_code,
           emp_code,
           date_modify,
           action)
        values
          (b_id,
           4,
           setup.get_operday,
           boshMfo,
           v_foiz_code,
           vCode_b,
           sysdate,
           'I');
        update qqb_hl_accounts h
           set h.acc_foiz_bo = v_acc_external
         where h.t2_id = iSubmanbaId
           and h.filial = iMfo;
      end if;
      --shu erga 44905,17101840,17101 larni ham biriktir
    exception
      when others then
        null;
    end;
--Бош банкда ДЮЛ га 6, 11, 12 - тип счетларни бириктириш
select s.currency
  into vCurrCode
  from QQB_HL_SPR_SUBMANBA s
 where s.id = iSubmanbaId;
if vCurrCode = '840' then
  vAccId_6 := '8101868';
  vAccId_11 := '2532784';
  vAccId_12 := '2094801';
elsif vCurrCode = '978' then
  vAccId_6 := '8101868';
  vAccId_11 := '8345511';
  vAccId_12 := '8345510';
end if;
-- 6-тип счет
begin
  select a.code, a.acc_external, a.id, a.code_coa
    into v_acc_code, v_acc_external, v_id, v_code_coa
    from accounts a
   where a.id = vAccId_6;
  if sql%found then
    update dep_accounts
       set date_next = setup.get_operday
     where contract_id = b_id
       and account_type = 6
       and date_next = to_date('31.12.9999', 'dd.mm.yyyy');
    insert into dep_accounts
      (contract_id,
       account_type,
       date_validate,
       filial_code,
       account_code,
       emp_code,
       date_modify,
       date_next,
       acc_id,
       coa)
    values
      (b_id,
       6,
       setup.get_operday,
       boshMfo,
       v_acc_code,
       vCode_b,
       sysdate,
       to_date('31.12.9999', 'dd.mm.yyyy'),
       v_id,
       v_code_coa);
    insert into dep_accounts_his
      (contract_id,
       account_type,
       date_validate,
       filial_code,
       account_code,
       emp_code,
       date_modify,
       action)
    values
      (b_id,
       6,
       setup.get_operday,
       boshMfo,
       v_acc_code,
       vCode_b,
       sysdate,
       'I');
  end if;
exception
  when others then
    null;
end;
-- 11-тип счет
begin
  select a.code, a.acc_external, a.id, a.code_coa
    into v_acc_code, v_acc_external, v_id, v_code_coa
    from accounts a
   where a.id = vAccId_11;
  if sql%found then
    update dep_accounts
       set date_next = setup.get_operday
     where contract_id = b_id
       and account_type = 11
       and date_next = to_date('31.12.9999', 'dd.mm.yyyy');
    insert into dep_accounts
      (contract_id,
       account_type,
       date_validate,
       filial_code,
       account_code,
       emp_code,
       date_modify,
       date_next,
       acc_id,
       coa)
    values
      (b_id,
       11,
       setup.get_operday,
       boshMfo,
       v_acc_code,
       vCode_b,
       sysdate,
       to_date('31.12.9999', 'dd.mm.yyyy'),
       v_id,
       v_code_coa);
    insert into dep_accounts_his
      (contract_id,
       account_type,
       date_validate,
       filial_code,
       account_code,
       emp_code,
       date_modify,
       action)
    values
      (b_id,
       11,
       setup.get_operday,
       boshMfo,
       v_acc_code,
       vCode_b,
       sysdate,
       'I');
  end if;
exception
  when others then
    null;
end;
-- 12-тип счет
begin
  select a.code, a.acc_external, a.id, a.code_coa
    into v_acc_code, v_acc_external, v_id, v_code_coa
    from accounts a
   where a.id = vAccId_12;
  if sql%found then
    update dep_accounts
       set date_next = setup.get_operday
     where contract_id = b_id
       and account_type = 12
       and date_next = to_date('31.12.9999', 'dd.mm.yyyy');
    insert into dep_accounts
      (contract_id,
       account_type,
       date_validate,
       filial_code,
       account_code,
       emp_code,
       date_modify,
       date_next,
       acc_id,
       coa)
    values
      (b_id,
       12,
       setup.get_operday,
       boshMfo,
       v_acc_code,
       vCode_b,
       sysdate,
       to_date('31.12.9999', 'dd.mm.yyyy'),
       v_id,
       v_code_coa);
    insert into dep_accounts_his
      (contract_id,
       account_type,
       date_validate,
       filial_code,
       account_code,
       emp_code,
       date_modify,
       action)
    values
      (b_id,
       12,
       setup.get_operday,
       boshMfo,
       v_acc_code,
       vCode_b,
       sysdate,
       'I');
  end if;
exception
  when others then
    null;
end;
    --Filialda asosiy schet(22203)
    begin
      select a.code, a.condition, a.id, a.code_coa
        into f_acc_code, v_acc_condition, v_id, v_code_coa
        from accounts a
       where a.code_filial = iMfo
         and a.acc_external = v_acc_res_fil;
    exception
      when no_data_found then
        --филиалда da 22203 ochish;
        setup.Set_Employee_Code(vCode_f);
        --tartib nomer
        if substr(v_acc_res_fil, -3) = '___' then
          begin
            select lpad(max(substr(a.acc_external, -3)) + 1, 3, '0')
              into tartibNo
              from accounts a
             where a.code_filial = iMfo
               and a.acc_external like
                   substr(v_acc_res_fil, 1, 9) || bosh_unqcode || '___';
          exception
            when no_data_found then
              tartibNo := '001';
          end;
        else
          tartibNo := substr(v_acc_res_fil, -3);
        end if;
        --schet nomi
        select t.name ||
               ' кредит линияси буйича бош банкдан олинган ресурслар'
          into account_name
          from QQB_HL_SPR_SUBMANBA t
         where t.id = iSubmanbaId;
        --
        oAccountCode := '11' || setup.get_local_code ||
                        substr(v_acc_res_fil, 1, 9) || bosh_unqcode ||
                        tartibNo;
        begin
          select a.code
            into f_acc_code
            from accounts a
           where a.code like oAccountCode;
        exception
          when no_data_found then
            select c.client_uid
              into vClUId
              from client_current c
             where c.code = filial_unqcode
               and rownum = 1;
            iOperation := 'A';
            account.action(vClUId,
                           oAccountCode,
                           iMfo,
                           account_name,
                           'P',
                           'B',
                           setup.get_operday,
                           iOperation,
                           oCondition,
                           oDialog,
                           oMessage);
            f_acc_code := oAccountCode;
          
        end;
    end;
    --ДЮЛ ва qqb_hl_accountsга Бириктириш
    begin
      select a.code, a.acc_external, a.id, a.code_coa
        into f_acc_code, v_acc_external, v_id, v_code_coa
        from accounts a
       where a.code = f_acc_code;
      if sql%found then
        update dep_accounts
           set date_next = setup.get_operday
         where contract_id = f_id
           and account_type = 1
           and date_next = to_date('31.12.9999', 'dd.mm.yyyy');
        insert into dep_accounts
          (contract_id,
           account_type,
           date_validate,
           filial_code,
           account_code,
           emp_code,
           date_modify,
           date_next,
           acc_id,
           coa)
        values
          (f_id,
           1,
           setup.get_operday,
           iMfo,
           f_acc_code,
           vCode_b,
           sysdate,
           to_date('31.12.9999', 'dd.mm.yyyy'),
           v_id,
           v_code_coa);
        insert into dep_accounts_his
          (contract_id,
           account_type,
           date_validate,
           filial_code,
           account_code,
           emp_code,
           date_modify,
           action)
        values
          (f_id,
           1,
           setup.get_operday,
           iMfo,
           f_acc_code,
           vCode_b,
           sysdate,
           'I');
        update qqb_hl_accounts h
           set h.acc_res_fil = v_acc_external
         where h.t2_id = iSubmanbaId
           and h.filial = iMfo;
        v_acc_res_fil := v_acc_external;
      end if;
    exception
      when others then
        null;
    end;
    --Filialda procent schet(22409)
    begin
      select a.code, a.condition, a.id, a.code_coa
        into f_foiz_code, v_acc_condition, v_id, v_code_coa
        from accounts a
       where a.code_filial = iMfo
         and a.acc_external = v_acc_foiz_fil;
    exception
      when no_data_found then
        --филиалда  22409 ochish;
        setup.Set_Employee_Code(vCode_f);
        --tartib nomer
        if substr(v_acc_foiz_fil, -3) = '___' then
          begin
            select lpad(max(substr(a.acc_external, -3)) + 1, 3, '0')
              into tartibNo
              from accounts a
             where a.code_filial = iMfo
               and a.acc_external like
                   substr(v_acc_foiz_fil, 1, 9) || bosh_unqcode || '___';
          exception
            when no_data_found then
              tartibNo := '001';
          end;
        else
          tartibNo := substr(v_acc_foiz_fil, -3);
        end if;
        --schet nomi
        select t.name || ' кредит линияси буйича ' || b.name ||
               'га берилган ресурсга хисоб.фоизлар '
          into account_name
          from QQB_HL_SPR_SUBMANBA t, bank_desc_glb b
         where t.id = iSubmanbaId
           and b.code = iMfo;
        --
        oAccountCode := '11' || setup.get_local_code ||
                        substr(v_acc_foiz_fil, 1, 9) || bosh_unqcode ||
                        tartibNo;
        begin
          select a.code
            into f_foiz_code
            from accounts a
           where a.code like oAccountCode;
        exception
          when no_data_found then
            select c.client_uid
              into vClUId
              from client_current c
             where c.code = filial_unqcode
               and rownum = 1;
            iOperation := 'A';
            account.action(vClUId,
                           oAccountCode,
                           iMfo,
                           account_name,
                           'P',
                           'B',
                           setup.get_operday,
                           iOperation,
                           oCondition,
                           oDialog,
                           oMessage);
            f_foiz_code := oAccountCode;
          
        end;
    end;
    --ДЮЛ ва qqb_hl_accountsга Бириктириш
    begin
      select a.code, a.acc_external, a.id, a.code_coa
        into f_foiz_code, v_acc_external, v_id, v_code_coa
        from accounts a
       where a.code = f_foiz_code;
      if sql%found then
        update dep_accounts
           set date_next = setup.get_operday
         where contract_id = f_id
           and account_type = 4
           and date_next = to_date('31.12.9999', 'dd.mm.yyyy');
        insert into dep_accounts
          (contract_id,
           account_type,
           date_validate,
           filial_code,
           account_code,
           emp_code,
           date_modify,
           date_next,
           acc_id,
           coa)
        values
          (f_id,
           4,
           setup.get_operday,
           iMfo,
           f_foiz_code,
           vCode_b,
           sysdate,
           to_date('31.12.9999', 'dd.mm.yyyy'),
           v_id,
           v_code_coa);
        insert into dep_accounts_his
          (contract_id,
           account_type,
           date_validate,
           filial_code,
           account_code,
           emp_code,
           date_modify,
           action)
        values
          (f_id,
           4,
           setup.get_operday,
           iMfo,
           f_foiz_code,
           vCode_b,
           sysdate,
           'I');
        update qqb_hl_accounts h
           set h.acc_foiz_fil = v_acc_external
         where h.t2_id = iSubmanbaId
           and h.filial = iMfo;
        --shu erga 5,17101840,17101 larni ham biriktir
      end if;
    exception
      when others then
        null;
    end;
    
  --Filialda 5- тип счет (54904000)
    begin
      select a.code, a.condition, a.id, a.code_coa
        into f_acc_code, v_acc_condition, v_id, v_code_coa
        from accounts a
       where a.code_filial = iMfo
         and a.code_coa = '54904'
         and a.code_currency = '000'
         and a.client_code = filial_unqcode
         and substr(a.acc_external, -3) = '103';
    exception
      when no_data_found then
        --филиалда da 54904 ochish;
        setup.Set_Employee_Code(vCode_f);
        --tartib nomer
        tartibNo := '103';
        --schet nomi 
        account_name:= 'ХТТБ ва ХТА ва бошка кредит линиялари хисобидан ажратилган ресурсларлари учун хисобланган фоизли харажатлар';
        --
        oAccountCode := '11' || setup.get_local_code || '54904000_' || filial_unqcode || tartibNo;
        begin
          select a.code
            into f_acc_code_5
            from accounts a
           where a.code like oAccountCode;
        exception
          when no_data_found then
            select c.client_uid
              into vClUId
              from client_current c
             where c.code = filial_unqcode
               and rownum = 1;
            iOperation := 'A';
            account.action(vClUId,
                           oAccountCode,
                           iMfo,
                           account_name,
                           'P',
                           'B',
                           setup.get_operday,
                           iOperation,
                           oCondition,
                           oDialog,
                           oMessage);
            f_acc_code_5 := oAccountCode;
        end;
    end;
    --ДЮЛ ва qqb_hl_accountsга Бириктириш
    begin
      select a.code, a.acc_external, a.id, a.code_coa
        into f_acc_code_5, v_acc_external, v_id, v_code_coa
        from accounts a
       where a.code = f_acc_code_5;
      if sql%found then
        update dep_accounts
           set date_next = setup.get_operday
         where contract_id = f_id
           and account_type = 5
           and date_next = to_date('31.12.9999', 'dd.mm.yyyy');
        insert into dep_accounts
          (contract_id,
           account_type,
           date_validate,
           filial_code,
           account_code,
           emp_code,
           date_modify,
           date_next,
           acc_id,
           coa)
        values
          (f_id,
           5,
           setup.get_operday,
           iMfo,
           f_acc_code_5,
           vCode_b,
           sysdate,
           to_date('31.12.9999', 'dd.mm.yyyy'),
           v_id,
           v_code_coa);
        insert into dep_accounts_his
          (contract_id,
           account_type,
           date_validate,
           filial_code,
           account_code,
           emp_code,
           date_modify,
           action)
        values
          (f_id,
           5,
           setup.get_operday,
           iMfo,
           f_acc_code_5,
           vCode_b,
           sysdate,
           'I');
        update qqb_hl_accounts h
           set h.acc_res_fil = v_acc_external
         where h.t2_id = iSubmanbaId
           and h.filial = iMfo;
      end if;
    exception
      when others then
        null;
    end;  
  --Filialda 11- тип счет (17101000)
    begin
      select a.code, a.condition, a.id, a.code_coa
        into f_acc_code, v_acc_condition, v_id, v_code_coa
        from accounts a
       where a.code_filial = iMfo
         and a.code_coa = '17101'
         and a.code_currency = '000'
         and a.client_code = bosh_unqcode
         and substr(a.acc_external, -3) = substr(v_acc_res_fil, -3);
    exception
      when no_data_found then
        --филиалда da 17101 ochish;
        setup.Set_Employee_Code(vCode_f);
        --tartib nomer
        tartibNo := substr(v_acc_res_fil, -3);
        --schet nomi
        select t.name ||
               ' кредит линияси буйича бош банкдан олинган ресурслар'
          into account_name
          from QQB_HL_SPR_SUBMANBA t
         where t.id = iSubmanbaId;
        --
        oAccountCode := '11' || setup.get_local_code || '17101000_' || bosh_unqcode || tartibNo;
        begin
          select a.code
            into f_acc_code_11
            from accounts a
           where a.code like oAccountCode;
        exception
          when no_data_found then
            select c.client_uid
              into vClUId
              from client_current c
             where c.code = filial_unqcode
               and rownum = 1;
            iOperation := 'A';
            account.action(vClUId,
                           oAccountCode,
                           iMfo,
                           account_name,
                           'P',
                           'B',
                           setup.get_operday,
                           iOperation,
                           oCondition,
                           oDialog,
                           oMessage);
            f_acc_code_11 := oAccountCode;
        end;
    end;
    --ДЮЛ ва qqb_hl_accountsга Бириктириш
    begin
      select a.code, a.acc_external, a.id, a.code_coa
        into f_acc_code_11, v_acc_external, v_id, v_code_coa
        from accounts a
       where a.code = f_acc_code_11;
      if sql%found then
        update dep_accounts
           set date_next = setup.get_operday
         where contract_id = f_id
           and account_type = 11
           and date_next = to_date('31.12.9999', 'dd.mm.yyyy');
        insert into dep_accounts
          (contract_id,
           account_type,
           date_validate,
           filial_code,
           account_code,
           emp_code,
           date_modify,
           date_next,
           acc_id,
           coa)
        values
          (f_id,
           11,
           setup.get_operday,
           iMfo,
           f_acc_code_11,
           vCode_b,
           sysdate,
           to_date('31.12.9999', 'dd.mm.yyyy'),
           v_id,
           v_code_coa);
        insert into dep_accounts_his
          (contract_id,
           account_type,
           date_validate,
           filial_code,
           account_code,
           emp_code,
           date_modify,
           action)
        values
          (f_id,
           11,
           setup.get_operday,
           iMfo,
           f_acc_code_11,
           vCode_b,
           sysdate,
           'I');
        update qqb_hl_accounts h
           set h.acc_res_fil = v_acc_external
         where h.t2_id = iSubmanbaId
           and h.filial = iMfo;
      end if;
    exception
      when others then
        null;
    end;
    --Filialda 12- тип счет (17101840)
    begin
      select a.code, a.condition, a.id, a.code_coa
        into f_acc_code, v_acc_condition, v_id, v_code_coa
        from accounts a
       where a.code_filial = iMfo
         and a.code_coa = '17101'
         and a.code_currency = '840'
         and a.client_code = bosh_unqcode
         and substr(a.acc_external, -3) = substr(v_acc_res_fil, -3);
    exception
      when no_data_found then
        --филиалда da 17101 ochish;
        setup.Set_Employee_Code(vCode_f);
        --tartib nomer
        tartibNo := substr(v_acc_res_fil, -3);
        --schet nomi
        select t.name ||
               ' кредит линияси буйича бош банкдан олинган ресурслар'
          into account_name
          from QQB_HL_SPR_SUBMANBA t
         where t.id = iSubmanbaId;
        --
        oAccountCode := '11' || setup.get_local_code || '17101840_' || bosh_unqcode || tartibNo;
        begin
          select a.code
            into f_acc_code_12
            from accounts a
           where a.code like oAccountCode;
        exception
          when no_data_found then
            select c.client_uid
              into vClUId
              from client_current c
             where c.code = filial_unqcode
               and rownum = 1;
            iOperation := 'A';
            account.action(vClUId,
                           oAccountCode,
                           iMfo,
                           account_name,
                           'P',
                           'B',
                           setup.get_operday,
                           iOperation,
                           oCondition,
                           oDialog,
                           oMessage);
            f_acc_code_12 := oAccountCode;
        end;
    end;
    --ДЮЛ ва qqb_hl_accountsга Бириктириш
    begin
      select a.code, a.acc_external, a.id, a.code_coa
        into f_acc_code_12, v_acc_external, v_id, v_code_coa
        from accounts a
       where a.code = f_acc_code_12;
      if sql%found then
        update dep_accounts
           set date_next = setup.get_operday
         where contract_id = f_id
           and account_type = 12
           and date_next = to_date('31.12.9999', 'dd.mm.yyyy');
        insert into dep_accounts
          (contract_id,
           account_type,
           date_validate,
           filial_code,
           account_code,
           emp_code,
           date_modify,
           date_next,
           acc_id,
           coa)
        values
          (f_id,
           12,
           setup.get_operday,
           iMfo,
           f_acc_code_12,
           vCode_b,
           sysdate,
           to_date('31.12.9999', 'dd.mm.yyyy'),
           v_id,
           v_code_coa);
        insert into dep_accounts_his
          (contract_id,
           account_type,
           date_validate,
           filial_code,
           account_code,
           emp_code,
           date_modify,
           action)
        values
          (f_id,
           12,
           setup.get_operday,
           iMfo,
           f_acc_code_12,
           vCode_b,
           sysdate,
           'I');
        update qqb_hl_accounts h
           set h.acc_res_fil = v_acc_external
         where h.t2_id = iSubmanbaId
           and h.filial = iMfo;
      end if;
    exception
      when others then
        null;
    end;
  end;
  --Nazarov X.A 28.09.2020
  procedure provodka(s_id glb_cred_hl_summa.id%type, oMessage out varchar2) is
    b_id             dep_contracts.id%type;
    f_id             dep_contracts.id%type;
    vCode_o          core_users.user_id%type;
    vCode_b          core_users.user_id%type;
    iis_preview_mode varchar2(3) := 'OFF';
    iContracts       array_varchar2;
    iParams_code     array_varchar2;
    iParams_value    array_varchar2;
    b_mfo            varchar2(5);
    b_sch            varchar2(27);
    b_sch16102       varchar2(20);
    f_mfo            varchar2(5);
    f_sch            varchar2(27);
    f_sch22203       varchar2(20);
    v_state          glb_cred_hl_summa.state%type;
    v_ruxsat         glb_cred_hl_summa.ruhsat_sum%type;
    v_nazn           leads_cur.pay_purpose%type;
    v_id_resurs      qqb_hl_resurs.id%type;
    vid_zayav        glb_cred_hl_summa.id_zayav%type;
    v_code_val       dep_contracts.currency_code%type;
------------------------------------------------------
    vOperDay         date:=setup.get_operday;
    vFarNumb         varchar2(10);
  begin
--------------------------Fozil---------------------------------
   begin
   select f.far_numb
     into vFarNumb
     from qqb_hl_spr_farmoyish f
    where f.far_date = vOperDay;
   exception when no_data_found then
     Raise_Application_Error(-20000, 'Аввал '||vOperDay||' сана учун фармойиш киритинг!(Настройки -> Фармойишлар)');
   end;
--------------------------Fozil---------------------------------
    begin
      select m.state, nvl(m.ruhsat_sum, 0), id_resurs, m.id_zayav
        into v_state, v_ruxsat, v_id_resurs, vid_zayav
        from glb_cred_hl_summa m
       where m.id = s_id;       
      if sql%found then
        if v_state <> 1 then
          Raise_Application_Error(-20000,
                                  'Рухсат берилмаган!');
        end if;
      end if;
    exception
      when others then
        Raise_Application_Error(-20000, sqlerrm);
    end;
    begin
      select r.dep_id_b, r.dep_id_f
        into b_id, f_id
        from qqb_hl_resurs r
       where r.id = v_id_resurs
         and r.zv_id = vid_zayav;
    exception
      when others then
        Raise_Application_Error(-20000, sqlerrm);
    end;
    --currency
    begin
      select d.currency_code
        into v_code_val
        from dep_contracts d
       where d.id = b_id;
    exception
      when no_data_found then
        oMessage := 'Контракт топилмади';
        return;
      when too_many_rows then
        oMessage := 'Контракт бир нечта';
        return;
      when others then
        oMessage := 'Контрактни аниклашда хатолик';
        return;
    end;
    --naznacheniya
    begin
      select (select d.contract_name || 'га '
                from dep_contracts d
               where d.id = r.dep_id_b) ||
             (select b.name || ' хорижий кредит линияси буйича '
                from qqb_hl_spr_submanba b
               where b.ID = r.submanba_id) ||
              '№ ' ||vFarNumb ||' фармойишга асосан '||
             (select d.count_days || ' кунга '
                from dep_contracts d
               where d.id = r.dep_id_b) ||
             (select 'йиллик ' || to_char(p.percent_rate) ||
                     ' фоиз ставкада ажратилган ресур'
                from dep_contracts_percent_rate p
               where p.contract_id = r.dep_id_b
                 and p.percent_type = 'DEP'
                 and p.date_validate =
                     (select max(p1.date_validate)
                        from dep_contracts_percent_rate p1
                       where p1.contract_id = p.contract_id
                         and p1.percent_type = p.percent_type)) procet
        into v_nazn
        from qqb_hl_resurs r
       where r.id = v_id_resurs;
    exception
      when no_data_found then
        oMessage := 'Тулов максади топилмади';
        return;
      when too_many_rows then
        oMessage := 'Тулов максади бир нечта';
        return;
      when others then
        oMessage := 'Тулов максади  хато';
        return;
    end;
    --Бош банк счетини (16102) аниклаш
    begin
      select d.filial_code, d.account_code, substr(d.account_code, -20)
        into b_mfo, b_sch, b_sch16102
        from dep_accounts d
       where d.contract_id = b_id
         and d.account_type = 1
         and d.date_next = to_date('31.12.9999', 'dd.mm.yyyy');
    exception
      when no_data_found then
        oMessage := 'bosh bank sceti topilmadi';
        return;
      when too_many_rows then
        oMessage := 'bosh bank sceti bir nechta';
        return;
      when others then
        oMessage := 'bosh bank sceti xato';
        return;
    end;
    --филиал  счетини (22203) аниклаш
    begin
      select d.filial_code, d.account_code, substr(d.account_code, -20)
        into f_mfo, f_sch, f_sch22203
        from dep_accounts d
       where d.contract_id = f_id
         and d.account_type = 1
         and d.date_next = to_date('31.12.9999', 'dd.mm.yyyy');
    exception
      when no_data_found then
        oMessage := 'filail sceti topilmadi';
        return;
      when too_many_rows then
        oMessage := 'filail sceti bir nechta';
        return;
      when others then
        oMessage := 'filail sceti xato';
        return;
    end;
    --Бош банк бош бухгалтер коди ва унга сессия(проводка килиш учун керак)
    select setup.get_employee_code into vCode_o from dual;
    select p.code
      into vCode_b
      from v_emp_pass p
     where p.filial_code = b_mfo
       and p.rank_code = 4
       and p.condition = 'A';
    setup.set_employee_code(vCode_b);
    --v_ruxsat суммага ресурс чикариш ,яъни проводка килиш (дт: 16102 к-т 22203)
    iContracts    := array_varchar2();
    iParams_code  := array_varchar2();
    iParams_value := array_varchar2();
    iContracts    := sv_budjet_zp.add_in_array(iContracts, b_id);
    iParams_code  := sv_budjet_zp.add_in_array(iParams_code, 'LEAD_ACCOUNT');
    iParams_value := sv_budjet_zp.add_in_array(iParams_value, f_sch22203);
    iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
                                               'LEAD_ACCOUNT_DEBET');
    iParams_value := sv_budjet_zp.add_in_array(iParams_value, b_sch16102);
    iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
                                               'LEAD_ACCOUNT_NAME');
    iParams_value := sv_budjet_zp.add_in_array(iParams_value,
                                               bank.get_account_name(f_sch));
    iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
                                               'LEAD_ACCOUNT_NAME_DEBET');
    iParams_value := sv_budjet_zp.add_in_array(iParams_value,
                                               bank.get_account_name(b_sch));
    iParams_code  := sv_budjet_zp.add_in_array(iParams_code, 'LEAD_ACT_ID');
    iParams_value := sv_budjet_zp.add_in_array(iParams_value, '11');
    iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
                                               'LEAD_DOC_NUMB');
    iParams_value := sv_budjet_zp.add_in_array(iParams_value, '1');
    iParams_code  := sv_budjet_zp.add_in_array(iParams_code, 'LEAD_FILIAL');
    iParams_value := sv_budjet_zp.add_in_array(iParams_value, b_mfo);
    iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
                                               'LEAD_FILIAL_CREDIT');
    iParams_value := sv_budjet_zp.add_in_array(iParams_value, f_mfo);
    iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
                                               'LEAD_FILIAL_NAME');
    iParams_value := sv_budjet_zp.add_in_array(iParams_value,
                                               'ТОШКЕНТ Ш., АТБ "КИШЛОК КУРИЛИШ БАНК" БОШ АМАЛИЁТЛАР БОШКАРМАСИ');
    iParams_code  := sv_budjet_zp.add_in_array(iParams_code, 'LEAD_INN');
    iParams_value := sv_budjet_zp.add_in_array(iParams_value,
                                               bank.get_inn_acc(f_sch));
    iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
                                               'LEAD_INN_DEBET');
    iParams_value := sv_budjet_zp.add_in_array(iParams_value,
                                               bank.get_inn_acc(b_sch));
    iParams_code  := sv_budjet_zp.add_in_array(iParams_code, 'LEAD_PURPOSE');
    iParams_value := sv_budjet_zp.add_in_array(iParams_value, v_nazn);
    iParams_code  := sv_budjet_zp.add_in_array(iParams_code, 'LEAD_SUM_PAY');
    iParams_value := sv_budjet_zp.add_in_array(iParams_value,
                                               trim(to_char(v_ruxsat,
                                                            '999999999999.99')));
    iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
                                               'LEAD_SUM_PAY_IN_WORDS');
    iParams_value := sv_budjet_zp.add_in_array(iParams_value,
                                               vkl_rep.summa_suz(100 *
                                                                 v_ruxsat,
                                                                 v_code_val));
    iParams_code  := sv_budjet_zp.add_in_array(iParams_code, 'LEAD_SYM_ID');
    iParams_value := sv_budjet_zp.add_in_array(iParams_value, '56061');
    iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
                                               'LEAD_TRANS_ID');
    iParams_value := sv_budjet_zp.add_in_array(iParams_value, '106');
    iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
                                               'LOAN_CURRENCY');
    iParams_value := sv_budjet_zp.add_in_array(iParams_value, v_code_val);
    BEGIN
      dep_operation.operation_action(oMessage,
                                     'PLACEMENT_RECORD_AMOUNT',
                                     'M',
                                     iContracts,
                                     iParams_code,
                                     iParams_value,
                                     iis_preview_mode);
    EXCEPTION
      WHEN OTHERS THEN
        FOR i in 1 .. iContracts.Count LOOP
          insert into vkl_proc_error
            (mfo, id_account, error_message, nachis_day)
          values
            (setup.Get_Filial_Code,
             iContracts(i),
             'HAND_dd' || oMessage,
             setup.Get_Operday);
        end loop;
    END;
    setup.set_employee_code(vCode_o);
  end;
  Procedure accept_test is
  
    v_dep_id_b          integer;
    v_dep_id_f          integer;
    v_error             varchar2(32767);
    v_action_resource   dep_s_action_resource.code%type;
    v_account_layouts   dep_account_layouts.id%type; --Макеты счетов; Межфил. депозит 16102-840
    v_deposit_type      dep_s_deposit_type.code%type; --Справочник типов вкладов; Межфилиальный депозит
    v_contract_date     dep_contracts.contract_date%type;
    v_date_end          dep_contracts.date_end%type;
    v_currency_code     dep_contracts.currency_code%type;
    v_desc_days_in_year DEP_s_DAYS_IN_YEAR.Code%type; --Справочник банковских дней;
  
    v_summa      dep_contracts.summa%type;
    v_cl_code    dep_contracts.client_code%type;
    v_loan_id    ln_card.loan_id%type;
    v_count_days dep_contracts.count_days%type;
    v_date_begin ln_card.open_date%type;
  
    vEmpCode                core_users.user_id%type; --current
    vEmpHeaderCode          core_users.user_id%type; --bosh bank
    vEmpZvCode              core_users.user_id%type; --zayavka
    v_mfo                   char(5);
    i_submanba_id           number;
    i_contract_name         dep_contracts.contract_name%type;
    i_contract_number       dep_contracts.contract_number%type;
    i_percent               DEP_CONTRACTS_PERCENT_RATE.PERCENT_RATE%type;
    i_percent_DATE_VALIDATE varchar2(255);
    i_Libor_check           varchar2(2);
    i_resurs_id             qqb_hl_resurs.id%type;
    o_Message               varchar2(255);
    tartibNo                char(10);
  
  begin
    begin
      select a.user_id
        into vEmpHeaderCode
        from core_users a
       where a.filial_code = '01037'
         and a.private_post_id = 4
         and a.state = 'A';
      v_date_begin := to_date(to_char(sysdate - 1, 'dd.mm.yyyy'),
                              'dd.mm.yyyy');
      v_date_end   := to_date(to_char(sysdate + 364, 'dd.mm.yyyy'),
                              'dd.mm.yyyy');
      SELECT v_date_end - v_date_begin into v_count_days FROM dual;
      v_contract_date         := v_date_begin;
      v_currency_code         := '840';
      v_desc_days_in_year     := 2;
      i_Libor_check           := 'N';
      i_percent_DATE_VALIDATE := '12.11.2020';
      for e in (SELECT *
                  FROM qqb_hl_resurs t
                 where t.zv_id in (-73)
                 order by t.zv_id desc) loop
        dbms_output.put_line('RESURS ID=' || e.id);
        v_mfo         := e.filial_code;
        v_summa       := e.summa;
        i_submanba_id := e.submanba_id;
        SELECT 'MF' || lpad(to_char(abs(e.zv_id)), 3, '0')
          into i_contract_number
          FROM dual;
        SELECT to_number(replace(e.izox, ',', '.'))
          into i_percent
          FROM dual;
      
        i_contract_name := v_mfo || '---' || i_contract_number;
        i_resurs_id     := e.id;
        tartibNo        := e.resurs_number;
        if i_submanba_id in (24, 27, 28, 29, 30) then
          v_currency_code := '978';
        end if;
        select a.user_id
          into vEmpZvCode
          from core_users a
         where a.filial_code = v_mfo
           and a.private_post_id = 4
           and a.state = 'A';
      
        ----Shartnoma; Размещение
        setup.set_employee_code(vEmpHeaderCode);
        v_action_resource := 'PLACEMENT';
        if v_currency_code = '978' then
          v_account_layouts := 74; --Макеты счетов; Межфил. депозит 16102-978
        end if;
        if v_currency_code = '840' then
          v_account_layouts := 63; --Макеты счетов; Межфил. депозит 16102-840
        end if;
        v_deposit_type := 4; --Справочник типов вкладов; Межфилиальный депозит
        contract_action(i_action_resource => v_action_resource,
                        
                        i_client_code           => lpad(v_mfo, 8, '0'),
                        i_contract_name         => i_contract_name,
                        i_contract_number       => i_contract_number,
                        i_account_layouts       => v_account_layouts,
                        i_deposit_type          => v_deposit_type,
                        i_contract_date         => v_contract_date,
                        i_date_begin            => v_date_begin,
                        i_date_end              => v_date_end,
                        i_summa                 => v_summa,
                        i_count_days            => v_count_days,
                        i_currency_code         => v_currency_code,
                        i_desc_days_in_year     => v_desc_days_in_year,
                        i_percent               => i_percent,
                        i_percent_DATE_VALIDATE => i_percent_DATE_VALIDATE,
                        i_Libor_check           => i_Libor_check,
                        o_oid                   => v_dep_id_b,
                        o_error                 => v_error);
        if v_error is not null then
          rollback;
          o_Message := v_error;
          Raise_Application_Error(-20000, v_error);
        else
          ----Shartnoma; Привлечение
          setup.set_employee_code(vEmpZvCode);
          if v_currency_code = '978' then
            v_account_layouts := 76; --Макеты счетов; Межфил. депозит 16102-978
          end if;
          if v_currency_code = '840' then
            v_account_layouts := 62; --Макеты счетов; Межфил. депозит 16102-840
          end if;
          v_action_resource := 'ATTRACT';
          v_deposit_type    := 4; --Справочник типов вкладов; Межфилиальный депозит
          contract_action(i_action_resource       => v_action_resource,
                          i_client_code           => '00001037',
                          i_contract_name         => i_contract_name,
                          i_contract_number       => i_contract_number,
                          i_account_layouts       => v_account_layouts,
                          i_deposit_type          => v_deposit_type,
                          i_contract_date         => v_contract_date,
                          i_date_begin            => v_date_begin,
                          i_date_end              => v_date_end,
                          i_summa                 => v_summa,
                          i_count_days            => v_count_days,
                          i_currency_code         => v_currency_code,
                          i_desc_days_in_year     => v_desc_days_in_year,
                          i_percent               => i_percent,
                          i_percent_DATE_VALIDATE => i_percent_DATE_VALIDATE,
                          i_Libor_check           => i_Libor_check,
                          o_oid                   => v_dep_id_f,
                          o_error                 => v_error);
        
          if v_error is not null then
            o_Message := v_error;
            rollback;
            Raise_Application_Error(-20000, v_error);
          else
            ---Shaxzod qo'shgan grafik genratsiya qiladi
            /*generator_graph(iLoan_id                => v_Loan_id,
            iDep_contract_id        => v_dep_id_b,
            IDep_filial_contract_id => v_dep_id_f);*/
          
            ---Деп Юр Лиц бириктириш
            update qqb_hl_resurs t
               set t.submanba_id = i_submanba_id,
                   t.dep_id_b    = v_dep_id_b,
                   t.dep_id_f    = v_dep_id_f,
                   t.state       = 2
             where t.id = i_resurs_id;
            --Nazarov X.A 28.09.2020
            --- Настройка режим работ
            --Головной офис
            insert into dep_contracts_mode_actions
              (contract_id,
               mode_calc_percent,
               mode_redemp_percent,
               deposit_delinquency_control,
               percent_delinquency_control,
               emp_code,
               date_modify,
               auto_close_contract,
               interest_methods,
               value_date)
            values
              (v_dep_id_b,
               'ACD',
               'MANUAL',
               'N',
               'N',
               vEmpHeaderCode,
               sysdate,
               'Y',
               'USUAL',
               'N');
            insert into dep_contracts_mode_actions_his
              (contract_id,
               mode_calc_percent,
               mode_redemp_percent,
               deposit_delinquency_control,
               percent_delinquency_control,
               emp_code,
               date_modify,
               auto_close_contract,
               interest_methods,
               value_date)
            values
              (v_dep_id_b,
               'ACD',
               'MANUAL',
               'N',
               'N',
               vEmpHeaderCode,
               sysdate,
               'Y',
               'USUAL',
               'N');
            --филиал
            insert into dep_contracts_mode_actions
              (contract_id,
               mode_calc_percent,
               mode_redemp_percent,
               deposit_delinquency_control,
               percent_delinquency_control,
               emp_code,
               date_modify,
               auto_close_contract,
               interest_methods,
               value_date)
            values
              (v_dep_id_f,
               'ACD',
               'MANUAL',
               'N',
               'N',
               vEmpZvCode,
               sysdate,
               'Y',
               'USUAL',
               'N');
            insert into dep_contracts_mode_actions_his
              (contract_id,
               mode_calc_percent,
               mode_redemp_percent,
               deposit_delinquency_control,
               percent_delinquency_control,
               emp_code,
               date_modify,
               auto_close_contract,
               interest_methods,
               value_date)
            values
              (v_dep_id_f,
               'ACD',
               'MANUAL',
               'N',
               'N',
               vEmpZvCode,
               sysdate,
               'Y',
               'USUAL',
               'N');
            --Счетлар бириктириш. йук булса очиб бириктириш
            openBindAcc(v_mfo,
                        i_submanba_id,
                        v_dep_id_b,
                        v_dep_id_f,
                        o_Message);
            -- Sherzodaka yozgan protokolni chaqirilgan
            qqb_resurs_val.prot(i_resurs_id,
                                'Рухсат берилди',
                                2,
                                3,
                                sysdate);
            --o_Message := 'Ma`lumotlar muvaffaqiyatli saqlandi';
          end if;
        end if;
      end loop;
    exception
      when others then
        o_Message := sqlerrm;
        Raise_Application_Error(-20000, sqlerrm);
    end;
  end accept_test;
  procedure openBindAcc_TEST(iMfo        varchar2,
                             iSubmanbaId integer,
                             b_id        dep_contracts.id%type,
                             f_id        dep_contracts.id%type,
                             oMessage    out varchar2,
                             tartibNo    varchar2) is
    v_acc_res_bo    accounts.acc_external%type;
    v_acc_foiz_bo   accounts.acc_external%type;
    v_acc_res_fil   accounts.acc_external%type;
    v_acc_foiz_fil  accounts.acc_external%type;
    v_acc_code      accounts.code%type;
    f_acc_code      accounts.code%type;
    v_acc_external  accounts.acc_external%type;
    v_acc_condition accounts.condition%type;
    vCode_b         v_emp_pass.code%type;
    vCode_f         v_emp_pass.code%type;
    iCondition      client_current.condition%type;
    vClUId          client_current.client_uid%type;
    filial_unqcode  accounts.client_code%type;
    bosh_unqcode    accounts.client_code%type := '00001037';
    account_name    accounts.name%type;
    oAccountCode    ACCOUNTS.CODE%TYPE;
    iOperation      S_ACC_OPERATION.OPERATION%TYPE;
    oCondition      S_ACC_OPERATION.CONDITION_RESULT%TYPE;
    oDialog         S_ACC_OPERATION.DIALOG_NEED%TYPE;
    boshMfo         varchar2(5) := '01037';
    v_id            accounts.id%type;
    v_code_coa      accounts.code_coa%type;
    v_foiz_code     accounts.code%type;
    f_foiz_code     accounts.code%type;
  begin
    select p.code
      into vCode_b
      from v_emp_pass p
     where p.filial_code = boshMfo
       and p.rank_code = 4
       and p.condition = 'A';
    select p.code
      into vCode_f
      from v_emp_pass p
     where p.filial_code = iMfo
       and p.rank_code = 4
       and p.condition = 'A';
    filial_unqcode := lpad(iMfo, 8, '0');
    begin
      select t.acc_res_bo, t.acc_foiz_bo, t.acc_res_fil, t.acc_foiz_fil
        into v_acc_res_bo, v_acc_foiz_bo, v_acc_res_fil, v_acc_foiz_fil
        from QQB_HL_ACCOUNTS t
       where t.filial = iMfo
         and t.t2_id = iSubmanbaId;
      if sql%found then
        select c.condition
          into iCondition
          from client_current c
         where c.code = filial_unqcode;
        if iCondition = 'T' then
          update client_current t
             set t.condition             = 'A',
                 t.date_validate         = sysdate,
                 t.date_change_condition = setup.get_operday
           where t.code = filial_unqcode;
          insert into client_history
            select * from client_current where code = filial_unqcode;
        end if;
        select c.condition
          into iCondition
          from client_current c
         where c.code = bosh_unqcode;
        if iCondition = 'T' then
          update client_current t
             set t.condition             = 'A',
                 t.date_validate         = sysdate,
                 t.date_change_condition = setup.get_operday
           where t.code = bosh_unqcode;
          insert into client_history
            select * from client_current where code = bosh_unqcode;
        end if;
      
      end if;
    exception
      when no_data_found then
        oMessage := 'QQB_HL_ACCOUNTS таблицада шаблон топилмади?!';
        return;
    end;
    --Bosh bankda asosiy schet(16102)
    begin
      select a.code, a.condition, a.id, a.code_coa
        into v_acc_code, v_acc_condition, v_id, v_code_coa
        from accounts a
       where a.code_filial = boshMfo
         and a.acc_external = v_acc_res_bo;
    exception
      when no_data_found then
        --01037 da 16102 ochish;
        setup.Set_Employee_Code(vCode_b);
        --schet nomi
        select substr(t.name, 1, 30) || ' кредит линияси буйича ' ||
               substr(b.name, 1, 30) || 'га берилган ресурслар'
          into account_name
          from QQB_HL_SPR_SUBMANBA t, bank_desc_glb b
         where t.id = iSubmanbaId
           and b.code = iMfo;
        --
        oAccountCode := '11' || setup.get_local_code ||
                        substr(v_acc_res_bo, 1, 9) || filial_unqcode ||
                        tartibNo;
        begin
          select a.code
            into v_acc_code
            from accounts a
           where a.code like oAccountCode;
        exception
          when no_data_found then
            select substr(oAccountCode, 17, 8) into vClUId from dual;
            iOperation := 'A';
            account.action(vClUId,
                           oAccountCode,
                           boshMfo,
                           account_name,
                           'A',
                           'B',
                           setup.get_operday,
                           iOperation,
                           oCondition,
                           oDialog,
                           oMessage);
            v_acc_code := oAccountCode;
          
        end;
    end;
    --ДЮЛ ва qqb_hl_accountsга Бириктириш
    begin
      select a.code, a.acc_external, a.id, a.code_coa
        into v_acc_code, v_acc_external, v_id, v_code_coa
        from accounts a
       where a.code = v_acc_code;
      if sql%found then
        update dep_accounts
           set date_next = setup.get_operday
         where contract_id = b_id
           and account_type = 1
           and date_next = to_date('31.12.9999', 'dd.mm.yyyy');
        insert into dep_accounts
          (contract_id,
           account_type,
           date_validate,
           filial_code,
           account_code,
           emp_code,
           date_modify,
           date_next,
           acc_id,
           coa)
        values
          (b_id,
           1,
           setup.get_operday,
           boshMfo,
           v_acc_code,
           vCode_b,
           sysdate,
           to_date('31.12.9999', 'dd.mm.yyyy'),
           v_id,
           v_code_coa);
        insert into dep_accounts_his
          (contract_id,
           account_type,
           date_validate,
           filial_code,
           account_code,
           emp_code,
           date_modify,
           action)
        values
          (b_id,
           1,
           setup.get_operday,
           boshMfo,
           v_acc_code,
           vCode_b,
           sysdate,
           'I');
        update qqb_hl_accounts h
           set h.acc_res_bo = v_acc_external
         where h.t2_id = iSubmanbaId
           and h.filial = iMfo;
      end if;
    exception
      when others then
        null;
    end;
    --Bosh bankda procent schet(16304)
    begin
      select a.code, a.condition, a.id, a.code_coa
        into v_foiz_code, v_acc_condition, v_id, v_code_coa
        from accounts a
       where a.code_filial = boshMfo
         and a.acc_external = v_acc_foiz_bo;
    exception
      when no_data_found then
        --01037 da 16304 ochish;
        setup.Set_Employee_Code(vCode_b);
        --schet nomi
        select substr(t.name, 1, 30) || ' кредит линияси буйича ' ||
               substr(b.name, 1, 30) ||
               'га берилган ресурсга хисоб.фоизлар '
          into account_name
          from QQB_HL_SPR_SUBMANBA t, bank_desc_glb b
         where t.id = iSubmanbaId
           and b.code = iMfo;
        --
        oAccountCode := '11' || setup.get_local_code ||
                        substr(v_acc_foiz_bo, 1, 9) || filial_unqcode ||
                        tartibNo;
        begin
          select a.code
            into v_foiz_code
            from accounts a
           where a.code like oAccountCode;
        exception
          when no_data_found then
            select substr(oAccountCode, 17, 8) into vClUId from dual;
            iOperation := 'A';
            account.action(vClUId,
                           oAccountCode,
                           boshMfo,
                           account_name,
                           'A',
                           'B',
                           setup.get_operday,
                           iOperation,
                           oCondition,
                           oDialog,
                           oMessage);
            v_foiz_code := oAccountCode;
          
        end;
    end;
    --ДЮЛ ва qqb_hl_accountsга Бириктириш
    begin
      select a.code, a.acc_external, a.id, a.code_coa
        into v_foiz_code, v_acc_external, v_id, v_code_coa
        from accounts a
       where a.code = v_foiz_code;
      if sql%found then
        update dep_accounts
           set date_next = setup.get_operday
         where contract_id = b_id
           and account_type = 4
           and date_next = to_date('31.12.9999', 'dd.mm.yyyy');
        insert into dep_accounts
          (contract_id,
           account_type,
           date_validate,
           filial_code,
           account_code,
           emp_code,
           date_modify,
           date_next,
           acc_id,
           coa)
        values
          (b_id,
           4,
           setup.get_operday,
           boshMfo,
           v_foiz_code,
           vCode_b,
           sysdate,
           to_date('31.12.9999', 'dd.mm.yyyy'),
           v_id,
           v_code_coa);
        insert into dep_accounts_his
          (contract_id,
           account_type,
           date_validate,
           filial_code,
           account_code,
           emp_code,
           date_modify,
           action)
        values
          (b_id,
           4,
           setup.get_operday,
           boshMfo,
           v_foiz_code,
           vCode_b,
           sysdate,
           'I');
        update qqb_hl_accounts h
           set h.acc_foiz_bo = v_acc_external
         where h.t2_id = iSubmanbaId
           and h.filial = iMfo;
      end if;
      --shu erga 44905,17101840,17101 larni ham biriktir
    exception
      when others then
        null;
    end;
    --Filialda asosiy schet(22203)
    begin
      select a.code, a.condition, a.id, a.code_coa
        into f_acc_code, v_acc_condition, v_id, v_code_coa
        from accounts a
       where a.code_filial = iMfo
         and a.acc_external = v_acc_res_fil;
    exception
      when no_data_found then
        --филиалда da 22203 ochish;
        setup.Set_Employee_Code(vCode_f);
        --schet nomi
        select substr(t.name, 1, 40) ||
               ' кредит линияси буйича бош банкдан олинган  ресурслар'
          into account_name
          from QQB_HL_SPR_SUBMANBA t
         where t.id = iSubmanbaId;
        --
        oAccountCode := '11' || setup.get_local_code ||
                        substr(v_acc_res_fil, 1, 9) || bosh_unqcode ||
                        tartibNo;
        begin
          select a.code
            into f_acc_code
            from accounts a
           where a.code like oAccountCode;
        exception
          when no_data_found then
            select substr(oAccountCode, 17, 8) into vClUId from dual;
            iOperation := 'A';
            account.action(vClUId,
                           oAccountCode,
                           iMfo,
                           account_name,
                           'P',
                           'B',
                           setup.get_operday,
                           iOperation,
                           oCondition,
                           oDialog,
                           oMessage);
            f_acc_code := oAccountCode;
          
        end;
    end;
    --ДЮЛ ва qqb_hl_accountsга Бириктириш
    begin
      select a.code, a.acc_external, a.id, a.code_coa
        into f_acc_code, v_acc_external, v_id, v_code_coa
        from accounts a
       where a.code = f_acc_code;
      if sql%found then
        update dep_accounts
           set date_next = setup.get_operday
         where contract_id = f_id
           and account_type = 1
           and date_next = to_date('31.12.9999', 'dd.mm.yyyy');
        insert into dep_accounts
          (contract_id,
           account_type,
           date_validate,
           filial_code,
           account_code,
           emp_code,
           date_modify,
           date_next,
           acc_id,
           coa)
        values
          (f_id,
           1,
           setup.get_operday,
           iMfo,
           f_acc_code,
           vCode_b,
           sysdate,
           to_date('31.12.9999', 'dd.mm.yyyy'),
           v_id,
           v_code_coa);
        insert into dep_accounts_his
          (contract_id,
           account_type,
           date_validate,
           filial_code,
           account_code,
           emp_code,
           date_modify,
           action)
        values
          (f_id,
           1,
           setup.get_operday,
           iMfo,
           f_acc_code,
           vCode_b,
           sysdate,
           'I');
        update qqb_hl_accounts h
           set h.acc_res_fil = v_acc_external
         where h.t2_id = iSubmanbaId
           and h.filial = iMfo;
      end if;
    exception
      when others then
        null;
    end;
    --Filialda procent schet(22409)
    begin
      select a.code, a.condition, a.id, a.code_coa
        into f_foiz_code, v_acc_condition, v_id, v_code_coa
        from accounts a
       where a.code_filial = iMfo
         and a.acc_external = v_acc_foiz_fil;
    exception
      when no_data_found then
        --филиалда  22409 ochish;
        setup.Set_Employee_Code(vCode_f);
        --schet nomi
        select substr(t.name, 1, 30) || ' кредит линияси буйича ' ||
               substr(b.name, 1, 30) ||
               'га берилган ресурсга хисоб.фоизлар '
          into account_name
          from QQB_HL_SPR_SUBMANBA t, bank_desc_glb b
         where t.id = iSubmanbaId
           and b.code = iMfo;
        --
        oAccountCode := '11' || setup.get_local_code ||
                        substr(v_acc_foiz_fil, 1, 9) || bosh_unqcode ||
                        tartibNo;
        begin
          select a.code
            into f_foiz_code
            from accounts a
           where a.code like oAccountCode;
        exception
          when no_data_found then
            select substr(oAccountCode, 17, 8) into vClUId from dual;
            iOperation := 'A';
            account.action(vClUId,
                           oAccountCode,
                           iMfo,
                           account_name,
                           'P',
                           'B',
                           setup.get_operday,
                           iOperation,
                           oCondition,
                           oDialog,
                           oMessage);
            f_foiz_code := oAccountCode;
          
        end;
    end;
    --ДЮЛ ва qqb_hl_accountsга Бириктириш
    begin
      select a.code, a.acc_external, a.id, a.code_coa
        into f_foiz_code, v_acc_external, v_id, v_code_coa
        from accounts a
       where a.code = f_foiz_code;
      if sql%found then
        update dep_accounts
           set date_next = setup.get_operday
         where contract_id = f_id
           and account_type = 4
           and date_next = to_date('31.12.9999', 'dd.mm.yyyy');
        insert into dep_accounts
          (contract_id,
           account_type,
           date_validate,
           filial_code,
           account_code,
           emp_code,
           date_modify,
           date_next,
           acc_id,
           coa)
        values
          (f_id,
           4,
           setup.get_operday,
           iMfo,
           f_foiz_code,
           vCode_b,
           sysdate,
           to_date('31.12.9999', 'dd.mm.yyyy'),
           v_id,
           v_code_coa);
        insert into dep_accounts_his
          (contract_id,
           account_type,
           date_validate,
           filial_code,
           account_code,
           emp_code,
           date_modify,
           action)
        values
          (f_id,
           4,
           setup.get_operday,
           iMfo,
           f_foiz_code,
           vCode_b,
           sysdate,
           'I');
        update qqb_hl_accounts h
           set h.acc_foiz_fil = v_acc_external
         where h.t2_id = iSubmanbaId
           and h.filial = iMfo;
        --shu erga 5,17101840,17101 larni ham biriktir
      end if;
    exception
      when others then
        null;
    end;
  
  end;
----------------------Fozil-----------------------------------------
  procedure far_add(iSon      qqb_hl_spr_farmoyish.far_numb%type,
                    iSana     qqb_hl_spr_farmoyish.far_date%type,
                    oMessage   out varchar2) is
    vID qqb_hl_spr_farmoyish.id%type := qqb_hl_spr_farmoyish_seq.nextval;
  begin
    select count(*)
      into vCount
      from qqb_hl_spr_farmoyish g
     where g.far_numb = iSon
       and g.far_date = iSana;
    if vCount = 0 then
      insert into qqb_hl_spr_farmoyish
        (id, far_date, far_numb, mod_date)
      values
        (vID, iSana, iSon, sysdate);
      oMessage := 'OK';
    else
      oMessage := 'No';
    end if;
  end;
----------------------Fozil-----------------------------------------

procedure vnebal_lead(iManba      number,
                      iSubManba   number,
                      iDtAcc      varchar2,
                      iKrAcc      varchar2,
                      iSumma      number,
                      iPayPurpose varchar2)
  is
  Leadid       number;
  s20206       accounts%rowtype;
  s29801       accounts%rowtype;
  vCode        core_users.user_id%type:=12376;
  vCodeOld     core_users.user_id%type;
  kk           varchar2(2);
  vOperDay     date:=setup.Get_Operday;
  Err_Code     number;
  Err_Msg      varchar2(2000);
  nazn         varchar2(2000);
  vMfo         varchar2(5):='01037';
  vCond        varchar2(1);
begin
 begin
  select setup.Get_Employee_Code into vCodeOld from dual;

  setup.Set_Employee_Code(vCode);      

  vkl_api.NEW_LEAD(doc_numb    => Leadid,
                   doc_date    => vOperDay,
                   cl_mfo      => vMfo,
                   cl_acc      => iDtAcc,
                   cl_inn      => bank.get_inn_acc(iDtAcc),
                   cl_name     => bank.Get_Account_Name(iDtAcc, 'N'),
                   co_mfo      => vMfo,
                   co_acc      => iKrAcc,
                   co_inn      => bank.get_inn_acc(iKrAcc),
                   co_name     => bank.Get_Account_Name(iKrAcc, 'N'),
                   pay_purpose => iPayPurpose,
                   sum_pay     => iSumma,
                   act_id      => 41,
                   trans_id    => 106,
                   op_dc       => 1,
                   sys_birth   => '8',
                   sys_id      => '6',
                   task_code   => 602,
                   Err_Code    => Err_Code,
                   Err_Msg     => Err_Msg,
                   Lead_id     => Leadid);
  if Err_Msg is not null then
    insert into FZ_DEP_ERR
    values (vMfo,
            iManba,
            substr(Get_Error_Msg_Short(err_Msg), 1, 240),
            sysdate,
            vOperDay,
            iSubManba);
  end if;

 exception when others then
  err_Msg :=substr(Sqlerrm, 1, 240);
  insert into FZ_DEP_ERR
  values (vMfo,
          iManba,
          err_Msg,
          sysdate,
          vOperDay,
          iSubManba);
  
  setup.Set_Employee_Code(vCodeOld);
 end;
end;

procedure uchet_vnebal(iManba          number,
                       iSubManba       number,
                       iPlusMinus      varchar2,
                       iSumma          number)
  is
  vVnebal        Qqb_Hl_Accounts_Vnebal%rowtype;
  vPlusOst       number;
  vMinusOst      number;
------------------------------
  vPlusDtAcc     accounts.code%type;
  vPlusKrAcc     accounts.code%type;
  vMinusDtAcc    accounts.code%type;
  vMinusKrAcc    accounts.code%type;
  vPlusSumma     number:=0;
  vMinusSumma    number:=0;
  
begin
  
  select *
    into vVnebal
    from Qqb_Hl_Accounts_Vnebal a
   where a.manba_id = iManba;
  select bank.Get_Saldo_Out(vVnebal.Acc_Plus_Deb)  into vPlusOst  from dual;
  select bank.Get_Saldo_Out(vVnebal.Acc_Minus_Deb) into vMinusOst from dual;
   
  if iPlusMinus = 'PLUS' then
    vPlusDtAcc  := vVnebal.Acc_Plus_Deb;
    vPlusKrAcc  := vVnebal.Acc_Plus_Krd;
    vMinusDtAcc := vVnebal.Acc_Minus_Krd;
    vMinusKrAcc := vVnebal.Acc_Minus_Deb;    
    if vMinusOst <> 0 then 
      if vMinusOst >=  iSumma then
        vPlusSumma  := 0;
        vMinusSumma := iSumma;
      else
        vPlusSumma  := iSumma - vMinusOst;
        vMinusSumma := vMinusOst;
      end if;
    else
      vPlusSumma := iSumma;
      vMinusSumma := 0;
    end if;
  end if;
  if iPlusMinus = 'MINUS' then
    vPlusDtAcc  := vVnebal.Acc_Plus_Krd;
    vPlusKrAcc  := vVnebal.Acc_Plus_Deb;
    vMinusDtAcc := vVnebal.Acc_Minus_Deb;
    vMinusKrAcc := vVnebal.Acc_Minus_Krd;    
    if vPlusOst <> 0 then
      if vPlusOst >=  iSumma then
        vPlusSumma  := iSumma;
        vMinusSumma := 0;        
      else
        vPlusSumma  := vPlusOst;
        vMinusSumma := iSumma - vPlusOst;
      end if;
    else
      vPlusSumma := 0;
      vMinusSumma := iSumma;
    end if;    
  end if;
  
  if vPlusSumma <> 0 then
    vnebal_lead(iManba, iSubManba, vPlusDtAcc, vPlusKrAcc, vPlusSumma, 'Test');
  end if;
  if vMinusSumma <> 0 then
    vnebal_lead(iManba, iSubManba, vMinusDtAcc, vMinusKrAcc, vMinusSumma, 'Test');
  end if;
  
  
end;


end qqb_resurs_val;
/
