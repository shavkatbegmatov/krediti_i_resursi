CREATE OR REPLACE PACKAGE QQB_RESURS_VAL IS
	PROCEDURE prot
	(
		iId_res         qqb_hl_resurs_protocol.id_res%TYPE
	   ,iIzox           qqb_hl_resurs_protocol.Izox%TYPE
	   ,iState_id       qqb_hl_resurs_protocol.state_id%TYPE
	   ,iOp_id          NUMBER
	   ,iDate_operation qqb_hl_resurs_protocol.date_operation%TYPE
	);
	PROCEDURE prot_index
	(
		iCode           qqb_hl_index_protocol.code%TYPE
	   ,iIzox           qqb_hl_index_protocol.Izox%TYPE
	   ,iState_id       qqb_hl_spr_state_type.id%TYPE
	   ,iCondition_id   qqb_hl_index_protocol.condition_id%TYPE
	   ,iDate_operation qqb_hl_index_protocol.date_operation%TYPE
	);
	PROCEDURE T3_template(iT2_Id IN qqb_hl_spr_submanba.id%TYPE);
	PROCEDURE request_add(REQUEST HASHTABLE);

	PROCEDURE sorov_tasdiq
	(
		iid           glb_cred_hl_summa.id%TYPE
	   ,iruhsat_state glb_cred_hl_summa.state%TYPE
	   ,isumma        glb_cred_hl_summa.ruhsat_sum%TYPE
	);
	PROCEDURE attach_dep(REQUEST HASHTABLE);
	--//manba add
	PROCEDURE manba_add
	(
		iName      qqb_hl_spr_manba.name%TYPE
	   ,iRevolver  qqb_hl_spr_manba.revolver%TYPE
	   ,iCondition qqb_hl_spr_manba.condition%TYPE
	   ,oMessage   OUT VARCHAR2
	);
	PROCEDURE rad_etish
	(
		iid   qqb_hl_resurs.id%TYPE
	   ,iizoh VARCHAR2
	   ,omess OUT VARCHAR2
	);
	PROCEDURE LIBOR_ACTION
	(
		IACTION        IN VARCHAR2
	   ,IDATE_VALIDATE IN qqb_hl_s_libors.DATE_VALIDATE%TYPE
	   ,IPERCENT_RATE  IN NUMBER
	);
	PROCEDURE manba_ed
	(
		iCode      qqb_hl_spr_manba.code%TYPE
	   ,iName      qqb_hl_spr_manba.name%TYPE
	   ,iRevolver  qqb_hl_spr_manba.revolver%TYPE
	   ,iCondition qqb_hl_spr_manba.condition%TYPE
	   ,oMessage   OUT VARCHAR2
	);
	PROCEDURE manba_active_disactive(iCode qqb_hl_spr_manba.code%TYPE);
	PROCEDURE detail_add
	(
		iManbaCode qqb_hl_spr_submanba.manba_code%TYPE
	   ,iName      qqb_hl_spr_submanba.name%TYPE
	   ,iSchet     qqb_hl_spr_submanba.schet%TYPE
	   ,iProc      qqb_hl_spr_submanba.foiz%TYPE
	   ,iType      qqb_hl_spr_submanba.type%TYPE
	   ,iCurr      qqb_hl_spr_submanba.currency%TYPE
	   ,oMessage   OUT VARCHAR2
	);
	PROCEDURE detail_ed
	(
		iId       qqb_hl_spr_submanba.id%TYPE
	   ,iName     qqb_hl_spr_submanba.name%TYPE
	   ,iSchet    qqb_hl_spr_submanba.schet%TYPE
	   ,iProc     qqb_hl_spr_submanba.foiz%TYPE
	   ,iType     qqb_hl_spr_submanba.type%TYPE
	   ,iCurrency qqb_hl_spr_submanba.currency%TYPE
	   ,oMessage  OUT VARCHAR2
	);
	PROCEDURE detail_del(iId qqb_hl_spr_submanba.id%TYPE);
	PROCEDURE index_add
	(
		iName      qqb_hl_indexes.name%TYPE
	   ,iCode_type qqb_hl_indexes.code_type%TYPE
	   ,oMessage   OUT VARCHAR2
	);
	PROCEDURE index_ed
	(
		iId      qqb_hl_indexes.id%TYPE
	   ,iName    qqb_hl_indexes.name%TYPE
	   ,oMessage OUT VARCHAR2
	);
	PROCEDURE index_info_add
	(
		iIndex_Id   qqb_hl_index_info.index_id%TYPE
	   ,iBegin_date qqb_hl_index_info.begin_date%TYPE
	   ,iEnd_date   qqb_hl_index_info.end_date%TYPE
	   ,iProcent    qqb_hl_index_info.procent%TYPE
	   ,oMessage    OUT VARCHAR2
	);
	PROCEDURE index_info_ed
	(
		iId         qqb_hl_index_info.id%TYPE
	   ,iBegin_date qqb_hl_index_info.begin_date%TYPE
	   ,iEnd_date   qqb_hl_index_info.end_date%TYPE
	   ,iProcent    qqb_hl_index_info.procent%TYPE
	   ,oMessage    OUT VARCHAR2
	);
	PROCEDURE detail_filial_ed
	(
		iId           qqb_hl_accounts.t2_id%TYPE
	   ,iFilial       qqb_hl_accounts.filial%TYPE
	   ,iAcc_res_bo   qqb_hl_accounts.acc_res_bo%TYPE
	   ,iAcc_foiz_bo  qqb_hl_accounts.acc_foiz_bo%TYPE
	   ,iAcc_res_fil  qqb_hl_accounts.acc_res_fil%TYPE
	   ,iAcc_foiz_fil qqb_hl_accounts.acc_foiz_fil%TYPE
	   ,oMessage      OUT VARCHAR2
	);
	PROCEDURE percent_rate_synch(iIndex_id qqb_hl_indexes.id%TYPE);
	FUNCTION is_invalid_date(iId qqb_hl_index_info.id%TYPE) RETURN NUMBER;
	FUNCTION Get_Sch_Name(iSch accounts.acc_external%TYPE) RETURN VARCHAR2;
	FUNCTION get_proc_acc
	(
		iMfo accounts.code_filial%TYPE
	   ,iAcc accounts.acc_external%TYPE
	) RETURN VARCHAR2;
	FUNCTION Get_Dep_Name(iDep_id dep_contracts.id%TYPE) RETURN VARCHAR2;
	FUNCTION Get_Dep_Name2(iDep_id dep_contracts.id%TYPE) RETURN VARCHAR2;
	FUNCTION Get_manba_Name
	(
		iZv_id glb_cred_zayav.id%TYPE
	   ,iType  NUMBER DEFAULT 0
	) RETURN VARCHAR2;
	FUNCTION Get_manba_Name1(iCode qqb_hl_spr_manba.code%TYPE) RETURN VARCHAR2;
	FUNCTION Get_manba_Name2(iCode qqb_hl_spr_manba.code%TYPE) RETURN VARCHAR2;
	FUNCTION Get_manba_Name3
	(
		iId   qqb_hl_spr_submanba.id%TYPE
	   ,iDate DATE
	) RETURN VARCHAR2;
	PROCEDURE SendToMain(iId_res qqb_hl_resurs.id%TYPE);
	PROCEDURE generator_graph
	(
		iLoan_id                ln_graph_debt.loan_id%TYPE
	   ,iDep_contract_id        dep_contracts.id%TYPE
	   ,IDep_filial_contract_id dep_contracts.id%TYPE
	);
	FUNCTION chek_schedules_graph(iDep_contract_id dep_contracts.id%TYPE)
		RETURN NUMBER;
	--------------------------------------
	----------------Azizbek---------------
	--------------------------------------
	PROCEDURE accept
	(
		i_submanba_id           NUMBER
	   ,i_contract_name         dep_contracts.contract_name%TYPE
	   ,i_contract_number       dep_contracts.contract_number%TYPE
	   ,i_contract_date         VARCHAR2
	   ,i_date_begin            VARCHAR2
	   ,i_date_end              VARCHAR2
	   ,i_percent               DEP_CONTRACTS_PERCENT_RATE.PERCENT_RATE%TYPE
	   ,i_percent_DATE_VALIDATE VARCHAR2
	   ,i_desc_days_in_year     DEP_s_DAYS_IN_YEAR.Code%TYPE
	   ,i_zv_id                 NUMBER
	   ,i_resurs_id             qqb_hl_resurs.id%TYPE
	   ,o_Message               OUT VARCHAR2
	);

	--------------------------------------
	-----------------Shams----------------
	--------------------------------------
	PROCEDURE createAccounts
	(
		iT2_ID      VARCHAR2
	   ,iFilial     VARCHAR2
	   ,iClientName VARCHAR2
	   ,oCondition  OUT VARCHAR2
	   ,oDialog     OUT VARCHAR2
	   ,oMessage    OUT VARCHAR2
	);
	--Nazarox X.A  28.09.2020 h/r biriktirish
	PROCEDURE openBindAcc
	(
		iMfo        VARCHAR2
	   ,iSubmanbaId INTEGER
	   ,b_id        dep_contracts.id%TYPE
	   ,f_id        dep_contracts.id%TYPE
	   ,oMessage    OUT VARCHAR2
	);
	--Nazarov X.A 28.09.2020
	PROCEDURE provodka
	(
		s_id     glb_cred_hl_summa.id%TYPE
	   ,oMessage OUT VARCHAR2
	);
	/*Procedure accept_test;*/
	PROCEDURE openBindAcc_TEST
	(
		iMfo        VARCHAR2
	   ,iSubmanbaId INTEGER
	   ,b_id        dep_contracts.id%TYPE
	   ,f_id        dep_contracts.id%TYPE
	   ,oMessage    OUT VARCHAR2
	   ,tartibNo    VARCHAR2
	);
	----------------------Fozil-----------------------------------------
	PROCEDURE far_add
	(
		iSon     qqb_hl_spr_farmoyish.far_numb%TYPE
	   ,iSana    qqb_hl_spr_farmoyish.far_date%TYPE
	   ,oMessage OUT VARCHAR2
	);
END QQB_RESURS_VAL;
/
CREATE OR REPLACE PACKAGE BODY qqb_resurs_val IS
	vResId qqb_hl_resurs.id%TYPE;

	vState_id   qqb_hl_spr_state_type.id%TYPE;
	vState_name qqb_hl_spr_state_type.name%TYPE;
	-- vOp_id          number;
	vCode_filial accounts.code_filial%TYPE;
	--vIzox           qqb_hl_resurs.izox%type;
	vDate           qqb_hl_resurs.resurs_date%TYPE;
	vDate_operation qqb_hl_resurs_protocol.date_operation%TYPE;
	vZvId           glb_cred_zayav.id%TYPE;
	-- vNumber         qqb_hl_resurs.resurs_number%type;
	vSumma     qqb_hl_resurs.summa%TYPE;
	vMuddat    qqb_hl_resurs.muddat%TYPE;
	vIndexName qqb_hl_spr_proc_type.name%TYPE;
	vCondition qqb_hl_spr_proc_type.condition%TYPE;
	vSchName   accounts.name%TYPE;
	vAcc       accounts.acc_external%TYPE;
	vCount     NUMBER;
	vResRow    qqb_hl_resurs%ROWTYPE;
	vZvRow     glb_cred_zayav%ROWTYPE;
	vCurrency  qqb_hl_spr_submanba.currency%TYPE;
	vText      qqb_hl_resurs_protocol.text%TYPE;
	vIdxInfRow qqb_hl_index_info%ROWTYPE;

	--//��������� ���������
	PROCEDURE prot
	(
		iId_res         qqb_hl_resurs_protocol.id_res%TYPE
	   ,iIzox           qqb_hl_resurs_protocol.Izox%TYPE
	   ,iState_id       qqb_hl_resurs_protocol.state_id%TYPE
	   ,iOp_id          NUMBER
	   ,iDate_operation qqb_hl_resurs_protocol.date_operation%TYPE
	) IS
		--vText       qqb_hl_resurs_protocol.text%type;
	BEGIN
		--vCode_filial := setup.get_filial_code;
		--vEmp_code    := setup.Get_Employee_Code;
		BEGIN
			IF iOp_id = 4
			THEN
				vtext := '��� ������';
			END IF;
			INSERT INTO qqb_hl_resurs_protocol
				(id_res,
				 code_filial,
				 emp_code,
				 date_operation,
				 text,
				 state_id,
				 izox)
			VALUES
				(iId_res,
				 setup.get_filial_code,
				 setup.get_employee_code,
				 iDate_operation,
				 vText,
				 iState_id,
				 iIzox);
		EXCEPTION
			WHEN OTHERS THEN
				Raise_Application_Error(-20000, SQLERRM);
		END;
	END prot;

	PROCEDURE prot_index
	(
		iCode           qqb_hl_index_protocol.code%TYPE
	   ,iIzox           qqb_hl_index_protocol.Izox%TYPE
	   ,iState_id       qqb_hl_spr_state_type.id%TYPE
	   ,iCondition_id   qqb_hl_index_protocol.condition_id%TYPE
	   ,iDate_operation qqb_hl_index_protocol.date_operation%TYPE
	) IS
		vEmp_code VARCHAR2(8);
	BEGIN
		vCode_filial := setup.Get_Filial_Code;
		vEmp_code    := setup.Get_Employee_Code;
		SELECT s.name
		  INTO vState_name
		  FROM qqb_hl_spr_state_type s
		 WHERE s.id = iState_id;
		SELECT i.name
		  INTO vIndexName
		  FROM qqb_hl_spr_proc_type i
		 WHERE i.code = iCode;
		vText := vState_name || ' ���������� ' || vIndexName;
		INSERT INTO qqb_hl_index_protocol
			(code, emp_code, date_operation, text, condition_id, izox)
		VALUES
			(iCode,
			 vEmp_code,
			 iDate_operation,
			 vText,
			 iCondition_id,
			 iIzox);
	END prot_index;

	--//T3 ������
	PROCEDURE T3_template(iT2_Id IN qqb_hl_spr_submanba.id%TYPE) IS
		vAcc_res_bo   qqb_hl_accounts.acc_res_bo%TYPE := '16102';
		vAcc_foiz_bo  qqb_hl_accounts.acc_foiz_bo%TYPE := '16304';
		vAcc_res_fil  qqb_hl_accounts.acc_res_fil%TYPE := '22203';
		vAcc_foiz_fil qqb_hl_accounts.acc_foiz_fil%TYPE := '22409';
	BEGIN
		IF iT2_Id IS NOT NULL
		THEN
			SELECT t2.currency
			  INTO vCurrency
			  FROM qqb_hl_spr_submanba t2
			 WHERE t2.id = iT2_Id;
			IF vCurrency IS NOT NULL
			THEN
				FOR f IN (SELECT t.code
							FROM bank_desc_glb t
						   WHERE t.code NOT IN ('00000', '00429', '09009')
								 AND t.code NOT IN
								 (SELECT c.filial
										FROM qqb_hl_accounts c
									   WHERE c.t2_id = iT2_Id))
				LOOP
					INSERT INTO qqb_hl_accounts
						(t2_id,
						 filial,
						 acc_res_bo,
						 acc_foiz_bo,
						 acc_res_fil,
						 acc_foiz_fil)
					VALUES
						(iT2_Id,
						 f.code,
						 vAcc_res_bo || vCurrency || '____________',
						 vAcc_foiz_bo || vCurrency || '____________',
						 vAcc_res_fil || vCurrency || '____________',
						 vAcc_foiz_fil || vCurrency || '____________');
				END LOOP;
			ELSE
				Raise_Application_Error(-20000,
										'������ ���� ������������!!!');
			END IF;
		ELSE
			Raise_Application_Error(-20000, 'ID ���');
		END IF;
	END T3_template;

	--//��������� �������
	------------------------------------______________
	---__Shaxzod qoshgan___
	--------------------------------------

	PROCEDURE request_add(REQUEST HASHTABLE) IS
		vmfo       VARCHAR2(5);
		v_loan_id  ln_card.loan_id%TYPE;
		v_currency ln_card.currency%TYPE;
		--
		vResId          qqb_hl_resurs.id%TYPE;
		vState_id       qqb_hl_spr_state_type.id%TYPE;
		vDate_operation qqb_hl_resurs_protocol.date_operation%TYPE;
		vZvId           glb_cred_zayav.id%TYPE;
		vSumma          qqb_hl_resurs.summa%TYPE;
		vSumma_hl       glb_cred_hl_summa.summa%TYPE;
	
		vMuddat qqb_hl_resurs.muddat%TYPE;
		vId     glb_cred_hl_summa.id%TYPE;
		vDate   DATE;
		vcount  NUMBER;
		--- 
	BEGIN
	
		vDate           := request.get_varchar2('sana');
		vZvId           := request.Get_number('zv_id');
		vId             := request.Get_number('id');
		vSumma_hl       := request.Get_number('summa');
		vState_id       := 0; --��������� � ��
		vDate_operation := SYSDATE;
	
		SELECT COUNT(*)
		  INTO vcount
		  FROM qqb_hl_resurs a
		 WHERE a.zv_id = vZvId;
		IF vcount = 0
		THEN
			SELECT t.zayav_summa, t.srok, t.mfo, t.loan_id
			  INTO vSumma, vMuddat, vmfo, v_loan_id
			  FROM glb_cred_zayav t
			 WHERE t.id = vZvId;
			IF v_loan_id IS NULL
			THEN
				Raise_Application_Error(-20000,
										'������ ���������� loan_id ���������������?!');
			END IF;
			SELECT t.currency
			  INTO v_currency
			  FROM ln_card t
			 WHERE t.loan_id = v_loan_id;
			IF v_currency = '000'
			THEN
				Raise_Application_Error(-20000,
										' ������ ���� ?!');
			END IF;
			BEGIN
				vResId := qqb_hl_resurs_seq.nextval;
				INSERT INTO qqb_hl_resurs
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
				VALUES
					(vResId,
					 vDate,
					 '',
					 vSumma,
					 vZvId,
					 NULL,
					 vState_id,
					 vDate_operation,
					 vMuddat,
					 '',
					 vmfo);
			EXCEPTION
				WHEN dup_val_on_index THEN
					Raise_Application_Error(-20000,
											vZvId ||
											': Bu zayavka junatilgan');
			END;
		ELSE
			SELECT a.id
			  INTO vResId
			  FROM qqb_hl_resurs a
			 WHERE a.zv_id = vZvId;
		END IF;
		BEGIN
			IF vId = 0
			THEN
				INSERT INTO glb_cred_hl_summa
					(id, id_zayav, data, summa, state, id_resurs)
				VALUES
					(glb_cred_hl_summa_seq.nextval,
					 vZvId,
					 vDate,
					 vSumma_hl,
					 0,
					 vResId);
			ELSE
				UPDATE glb_cred_hl_summa d
				   SET d.data = vDate, d.summa = vSumma_hl
				 WHERE d.id = vId;
			END IF;
		EXCEPTION
			WHEN OTHERS THEN
				Raise_Application_Error(-20000, SQLERRM);
		END;
	
	END request_add;
	-----------------------------------------------------------------------------
	PROCEDURE sorov_tasdiq
	(
		iid           glb_cred_hl_summa.id%TYPE
	   ,iruhsat_state glb_cred_hl_summa.state%TYPE
	   ,isumma        glb_cred_hl_summa.ruhsat_sum%TYPE
	) IS
		vIdList        array_varchar2 := Array_Varchar2();
		vCred_hl_summa glb_cred_hl_summa%ROWTYPE;
		vHl_resurs     qqb_hl_resurs%ROWTYPE;
		vCode_o        core_users.user_id%TYPE;
		vCode_b        core_users.user_id%TYPE;
		oMessage       VARCHAR2(1000);
	BEGIN
		BEGIN
			UPDATE glb_cred_hl_summa h
			   SET h.ruhsat_sum = isumma, h.state = iruhsat_state
			 WHERE h.id = iid;
			IF iruhsat_state = 1
			THEN
				SELECT *
				  INTO vCred_hl_summa
				  FROM glb_cred_hl_summa a
				 WHERE a.id = iid;
				SELECT *
				  INTO vHl_resurs
				  FROM qqb_hl_resurs r
				 WHERE r.id = vCred_hl_summa.Id_Resurs;
				SELECT setup.get_employee_code INTO vCode_o FROM dual;
				SELECT p.code
				  INTO vCode_b
				  FROM v_emp_pass p
				 WHERE p.filial_code = '01037'
					   AND p.rank_code = 4
					   AND p.condition = 'A';
				setup.set_employee_code(vCode_b);
				vIdList.extend;
				vIdList(1) := vHl_resurs.Dep_Id_b;
				Dep_Api.contract_action(omessage => oMessage,
										iaction  => 'APPROVE',
										iid_list => vIdList);
				SELECT p.code
				  INTO vCode_b
				  FROM v_emp_pass p
				 WHERE p.filial_code = vHl_resurs.Filial_Code
					   AND p.rank_code = 4
					   AND p.condition = 'A';
				setup.set_employee_code(vCode_b);
				vIdList.Delete;
				vIdList.extend;
				vIdList(1) := vHl_resurs.Dep_Id_b;
				Dep_Api.contract_action(omessage => oMessage,
										iaction  => 'APPROVE',
										iid_list => vIdList);
				setup.set_employee_code(vCode_o);
			END IF;
		EXCEPTION
			WHEN OTHERS THEN
				Raise_Application_Error(-20000, SQLERRM);
		END;
	END;

	PROCEDURE rad_etish
	(
		iid   qqb_hl_resurs.id%TYPE
	   ,iizoh VARCHAR2
	   ,omess OUT VARCHAR2
	) IS
	BEGIN
		UPDATE qqb_hl_resurs a
		   SET a.izox = iizoh, a.state = 4
		 WHERE a.id = iid;
		omess := 'OK';
		prot(iId_res         => iid,
			 iIzox           => iizoh,
			 iState_id       => 4,
			 iOp_id          => '4',
			 iDate_operation => SYSDATE);
	
	END;
	---libor action
	PROCEDURE LIBOR_ACTION
	(
		IACTION        IN VARCHAR2
	   ,IDATE_VALIDATE IN qqb_hl_s_libors.DATE_VALIDATE%TYPE
	   ,IPERCENT_RATE  IN NUMBER
	) IS
		EX            EXCEPTION;
		VMESSAGE      VARCHAR2(4000);
		VSYSDATE      DATE;
		VEMP          qqb_hl_s_libors.EMP_CODE%TYPE;
		VID           NUMBER(5);
		v_date_active DATE := IDATE_VALIDATE;
		v_rate        NUMBER(2) := IPERCENT_RATE;
	BEGIN
	
		VEMP     := SETUP.GET_EMPLOYEE_CODE;
		VSYSDATE := SYSDATE;
	
		IF IDATE_VALIDATE IS NULL
		THEN
			VMESSAGE := '���� ������������';
			RAISE EX;
		END IF;
		BEGIN
		
			IF IACTION = 'ENTER'
			THEN
				VID := qqb_hl_s_libors_seq.nextval;
				BEGIN
					INSERT INTO qqb_hl_s_libors
						(DATE_VALIDATE,
						 PERCENT_RATE,
						 EMP_CODE,
						 DATE_MODIFY,
						 id)
					VALUES
						(IDATE_VALIDATE,
						 NVL(IPERCENT_RATE, 0),
						 SETUP.GET_EMPLOYEE_CODE,
						 VSYSDATE,
						 VID);
				
				EXCEPTION
					WHEN DUP_VAL_ON_INDEX THEN
						VMESSAGE := '���������� ' ||
									TO_CHAR(IDATE_VALIDATE,
											DEP_CONST.MASKDATE);
						RAISE EX;
				END;
			
			ELSIF IACTION = 'UPDATE'
			THEN
				SELECT A.ID
				  INTO vid
				  FROM qqb_hl_s_libors A
				 WHERE A.DATE_VALIDATE = IDATE_VALIDATE;
				UPDATE qqb_hl_s_libors T
				   SET T.PERCENT_RATE = NVL(IPERCENT_RATE, 0),
					   T.EMP_CODE     = VEMP,
					   T.DATE_MODIFY  = VSYSDATE
				 WHERE T.DATE_VALIDATE = IDATE_VALIDATE;
			
			ELSIF IACTION = 'DELETE'
			THEN
				/* UPDATE qqb_hl_s_libors T
                      SET T.EMP_CODE = VEMP, T.DATE_MODIFY = VSYSDATE
                    WHERE T.DATE_VALIDATE = IDATE_VALIDATE;
                */
				DELETE qqb_hl_s_libors T
				 WHERE T.DATE_VALIDATE = IDATE_VALIDATE;
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
			-- LoanId ��������� �����
			/*    fozil_actions.ln_percent_update(iLoanId => ,
                                                iTypeAction => 'OLD' );
            */
		EXCEPTION
			WHEN EX THEN
				RAISE_APPLICATION_ERROR(-20000, VMESSAGE);
		END;
	
		INSERT INTO qqb_hl_libor_his
			(date_validate,
			 percent_rate,
			 emp_code,
			 date_modify,
			 action,
			 id_libor)
		VALUES
			(IDATE_VALIDATE,
			 NVL(IPERCENT_RATE, 0),
			 setup.get_employee_code,
			 VSYSDATE,
			 IACTION,
			 VID);
	END LIBOR_ACTION;

	-------------------------------------------------------------------
	------------------------------------------------------------------

	--//��� �� ��� ����������
	PROCEDURE attach_dep(REQUEST HASHTABLE) IS
	BEGIN
		vResRow.Id            := request.Get_number('res_id');
		vResRow.Submanba_Id   := request.Get_number('submanba_id');
		vResRow.Dep_Id_b      := request.Get_number('dep_id_b');
		vResRow.Dep_Id_f      := request.Get_number('dep_id_f');
		vResRow.Izox          := request.Get_varchar2('izox');
		vResRow.Modified_Date := SYSDATE;
		UPDATE qqb_hl_resurs t
		   SET t.submanba_id = vResRow.Submanba_Id,
			   t.dep_id_b    = vResRow.Dep_Id_b,
			   t.dep_id_f    = vResRow.Dep_Id_f
		 WHERE t.id = vResRow.Id;
		prot(vResRow.Id, vResRow.Izox, 2, 3, vResRow.Modified_Date);
	END attach_dep;
	--//manba add
	PROCEDURE manba_add
	(
		iName      qqb_hl_spr_manba.name%TYPE
	   ,iRevolver  qqb_hl_spr_manba.revolver%TYPE
	   ,iCondition qqb_hl_spr_manba.condition%TYPE
	   ,oMessage   OUT VARCHAR2
	) IS
		vCode qqb_hl_spr_manba.code%TYPE := qqb_hr_spr_manba_seq.nextval;
	BEGIN
		SELECT COUNT(*)
		  INTO vCount
		  FROM qqb_hl_spr_manba g
		 WHERE g.name = iName;
		IF vCount = 0
		THEN
			INSERT INTO qqb_hl_spr_manba
				(code, NAME, revolver, condition)
			VALUES
				(vCode, iName, iRevolver, iCondition);
			oMessage := 'OK';
		ELSE
			oMessage := 'No';
		END IF;
	END manba_add;
	--//manba edit
	PROCEDURE manba_ed
	(
		iCode      qqb_hl_spr_manba.code%TYPE
	   ,iName      qqb_hl_spr_manba.name%TYPE
	   ,iRevolver  qqb_hl_spr_manba.revolver%TYPE
	   ,iCondition qqb_hl_spr_manba.condition%TYPE
	   ,oMessage   OUT VARCHAR2
	) IS
	BEGIN
		SELECT COUNT(*)
		  INTO vCount
		  FROM qqb_hl_spr_manba g
		 WHERE g.name = iName
			   AND g.code > 50
			   AND g.code != iCode;
		IF vCount = 0
		THEN
			UPDATE qqb_hl_spr_manba g
			   SET g.name      = iName,
				   g.revolver  = iRevolver,
				   g.condition = iCondition
			 WHERE g.code = iCode;
			oMessage := 'OK';
		ELSE
			oMessage := 'error';
		END IF;
	END manba_ed;
	--//manba active_disactive
	PROCEDURE manba_active_disactive(iCode qqb_hl_spr_manba.code%TYPE) IS
		vCondition qqb_hl_spr_manba.condition%TYPE := 'P';
	BEGIN
		SELECT g.condition
		  INTO vCondition
		  FROM qqb_hl_spr_manba g
		 WHERE g.code = iCode;
		IF vCondition = 'A'
		THEN
			UPDATE qqb_hl_spr_manba g
			   SET g.condition = 'P'
			 WHERE g.code = iCode;
		ELSE
			UPDATE qqb_hl_spr_manba g
			   SET g.condition = 'A'
			 WHERE g.code = iCode;
		END IF;
	END manba_active_disactive;
	--//qqb_hl_spr_submanba ga kiritish
	PROCEDURE detail_add
	(
		iManbaCode qqb_hl_spr_submanba.manba_code%TYPE
	   ,iName      qqb_hl_spr_submanba.name%TYPE
	   ,iSchet     qqb_hl_spr_submanba.schet%TYPE
	   ,iProc      qqb_hl_spr_submanba.foiz%TYPE
	   ,iType      qqb_hl_spr_submanba.type%TYPE
	   ,iCurr      qqb_hl_spr_submanba.currency%TYPE
	   ,oMessage   OUT VARCHAR2
	) IS
		vSubManbaId  qqb_hl_spr_submanba.id%TYPE := qqb_hl_spr_submanba_seq.nextval;
		vSubManbaVId qqb_hl_spr_submanba_vneb.id%TYPE := qqb_hl_spr_submanba_vneb_seq.nextval;
	BEGIN
		SELECT COUNT(*)
		  INTO vCount
		  FROM qqb_hl_spr_submanba t
		 WHERE t.name = iName;
		IF vCount = 0
		THEN
			INSERT INTO qqb_hl_spr_submanba
				(id, manba_code, NAME, foiz, schet, TYPE, currency)
			VALUES
				(vSubManbaId,
				 iManbaCode,
				 iName,
				 iProc,
				 iSchet,
				 iType,
				 iCurr);
			oMessage := 'OK';
		ELSE
			oMessage := 'No';
		END IF;
	END detail_add;
	--//qqb_hl_spr_submanba edit
	PROCEDURE detail_ed
	(
		iId       qqb_hl_spr_submanba.id%TYPE
	   ,iName     qqb_hl_spr_submanba.name%TYPE
	   ,iSchet    qqb_hl_spr_submanba.schet%TYPE
	   ,iProc     qqb_hl_spr_submanba.foiz%TYPE
	   ,iType     qqb_hl_spr_submanba.type%TYPE
	   ,iCurrency qqb_hl_spr_submanba.currency%TYPE
	   ,oMessage  OUT VARCHAR2
	) IS
		vSubManbaVId qqb_hl_spr_submanba_vneb.id%TYPE;
	BEGIN
		SELECT COUNT(*)
		  INTO vCount
		  FROM qqb_hl_spr_submanba t
		 WHERE t.name = iName
			   AND t.id != iId;
		IF vCount = 0
		THEN
			UPDATE qqb_hl_spr_submanba t
			   SET t.name     = iName,
				   t.schet    = iSchet,
				   t.foiz     = iProc,
				   t.type     = iType,
				   t.currency = iCurrency
			 WHERE t.id = iId;
			oMessage := 'OK';
		ELSE
			oMessage := 'error';
		END IF;
	END detail_ed;
	--//qqb_hl_spr_submanba delete
	PROCEDURE detail_del(iId qqb_hl_spr_submanba.id%TYPE) IS
	BEGIN
		DELETE FROM qqb_hl_spr_submanba t WHERE t.id = iId;
	END detail_del;

	PROCEDURE index_add
	(
		iName      qqb_hl_indexes.name%TYPE
	   ,iCode_type qqb_hl_indexes.code_type%TYPE
	   ,oMessage   OUT VARCHAR2
	) IS
		vId qqb_hl_indexes.id%TYPE;
	BEGIN
		SELECT COUNT(*)
		  INTO vCount
		  FROM qqb_hl_indexes t
		 WHERE t.name = iName;
		IF vCount = 0
		THEN
			vId             := qqb_hl_indexes_seq.nextval;
			vDate_operation := SYSDATE;
			INSERT INTO qqb_hl_indexes
				(id, code_type, NAME)
			VALUES
				(vId, iCode_type, iName);
			oMessage := 'OK';
		ELSE
			oMessage := 'No';
		END IF;
		--prot_index(iCode, iIzox, '5', iCondition, vDate_operation);
	END index_add;

	PROCEDURE index_ed
	(
		iId      qqb_hl_indexes.id%TYPE
	   ,iName    qqb_hl_indexes.name%TYPE
	   ,oMessage OUT VARCHAR2
	) IS
	BEGIN
		SELECT COUNT(*)
		  INTO vCount
		  FROM qqb_hl_indexes t
		 WHERE t.name = iName
			   AND t.id != iId;
		IF vCount = 0
		THEN
			vDate_operation := SYSDATE;
			UPDATE qqb_hl_indexes t SET t.name = iName WHERE t.id = iId;
			oMessage := 'OK';
			--prot_index(iCode, iIzox, '5', iCondition, vDate_operation);
		ELSE
			oMessage := 'No';
		END IF;
	END index_ed;

	PROCEDURE index_info_add
	(
		iIndex_Id   qqb_hl_index_info.index_id%TYPE
	   ,iBegin_date qqb_hl_index_info.begin_date%TYPE
	   ,iEnd_date   qqb_hl_index_info.end_date%TYPE
	   ,iProcent    qqb_hl_index_info.procent%TYPE
	   ,oMessage    OUT VARCHAR2
	) IS
	BEGIN
		INSERT INTO qqb_hl_index_info
			(id, index_id, begin_date, end_date, procent, created_on)
		VALUES
			(qqb_hl_index_info_seq.nextval,
			 iIndex_Id,
			 iBegin_date,
			 iEnd_date,
			 iProcent,
			 SYSDATE);
		oMessage := 'OK';
	EXCEPTION
		WHEN OTHERS THEN
			oMessage := 'No';
	END index_info_add;

	PROCEDURE index_info_ed
	(
		iId         qqb_hl_index_info.id%TYPE
	   ,iBegin_date qqb_hl_index_info.begin_date%TYPE
	   ,iEnd_date   qqb_hl_index_info.end_date%TYPE
	   ,iProcent    qqb_hl_index_info.procent%TYPE
	   ,oMessage    OUT VARCHAR2
	) IS
	BEGIN
		UPDATE qqb_hl_index_info t
		   SET t.begin_date = iBegin_date,
			   t.end_date   = iEnd_date,
			   t.procent    = iProcent,
			   t.created_on = SYSDATE
		 WHERE t.id = iId;
		oMessage := 'OK';
	EXCEPTION
		WHEN OTHERS THEN
			oMessage := 'No';
	END index_info_ed;

	PROCEDURE update_dep_percent_rate
	(
		iContract_id        NUMBER
	   ,iDate_validate_list array_varchar2
	   ,iPercent_rate_list  array_varchar2
	) IS
		vPercent_type_list array_varchar2 := array_varchar2();
		vlibor_check       array_varchar2 := array_varchar2();
		v_ARRAY_ACCESS     array_varchar2;
	BEGIN
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
	END update_dep_percent_rate;

	PROCEDURE percent_rate_synch(iIndex_id qqb_hl_indexes.id%TYPE) IS
		vContract_date dep_contracts.date_begin%TYPE;
		vDate_validate array_varchar2 := array_varchar2();
		vPercent_rate  array_varchar2 := array_varchar2();
		v_percent      VARCHAR2(20);
	
	BEGIN
		FOR i IN (SELECT t.dep_id_b
					FROM qqb_hl_resurs t
				   WHERE t.dep_id_b = '377310'
						 AND t.submanba_id IN
						 (SELECT s.id
								FROM qqb_hl_spr_submanba s
							   WHERE s.type = iIndex_id))
		LOOP
		
			FOR rates IN (SELECT to_char(r.begin_date, 'dd.mm.yyyy') begin_date,
								 r.procent
							FROM qqb_hl_index_info r
						   WHERE r.index_id = iIndex_id
						   ORDER BY r.begin_date)
			LOOP
				vDate_validate.Extend;
				vDate_validate(vDate_validate.Last) := rates.begin_date;
				SELECT REPLACE(decode(substr(to_char(rates.procent), 1, 1),
									  ',',
									  '0' || to_char(rates.procent),
									  to_char(rates.procent)),
							   ',',
							   '.')
				  INTO v_percent
				  FROM dual;
				vPercent_rate.Extend;
				vPercent_rate(vPercent_rate.Last) := v_percent;
				SELECT c.contract_date
				  INTO vContract_date
				  FROM dep_contracts c
				 WHERE c.id = i.dep_id_b;
				--if (vDate_validate.Last < vContract_date) then
				update_dep_percent_rate(i.dep_id_b,
										vDate_validate,
										vPercent_rate);
				--        end if;
				vPercent_rate  := NULL;
				vDate_validate := NULL;
			END LOOP;
		END LOOP;
	
	END percent_rate_synch;

	--//����������� ������� ����� ���������� ����������
	PROCEDURE detail_filial_ed
	(
		iId           qqb_hl_accounts.t2_id%TYPE
	   ,iFilial       qqb_hl_accounts.filial%TYPE
	   ,iAcc_res_bo   qqb_hl_accounts.acc_res_bo%TYPE
	   ,iAcc_foiz_bo  qqb_hl_accounts.acc_foiz_bo%TYPE
	   ,iAcc_res_fil  qqb_hl_accounts.acc_res_fil%TYPE
	   ,iAcc_foiz_fil qqb_hl_accounts.acc_foiz_fil%TYPE
	   ,oMessage      OUT VARCHAR2
	) IS
	BEGIN
		IF (iAcc_foiz_bo IS NULL)
		THEN
			oMessage := '������� ��.���. �������������� ' || iAcc_res_bo ||
						' ������ ����� �������� ���� ����� ������ ���������������!';
		ELSIF (iAcc_foiz_fil IS NULL)
		THEN
			oMessage := '������� ��.���. �������������� ' || iAcc_res_fil ||
						' ������ ����� �������� ���� ����� ������ ���������������!';
		ELSE
			UPDATE qqb_hl_accounts r
			   SET r.acc_res_bo   = iAcc_res_bo,
				   r.acc_foiz_bo  = iAcc_foiz_bo,
				   r.acc_res_fil  = iAcc_res_fil,
				   r.acc_foiz_fil = iAcc_foiz_fil
			 WHERE r.t2_id = iId
				   AND r.filial = iFilial;
			oMessage := 'OK';
		END IF;
	END detail_filial_ed;
	--//confirm ���������
	PROCEDURE confirm
	(
		iId_res  IN qqb_hl_resurs.id%TYPE
	   ,oMessage OUT VARCHAR2
	) IS
	BEGIN
		vDate_operation := SYSDATE;
		SELECT r.* INTO vResRow FROM qqb_hl_resurs r WHERE r.id = iId_res;
		SELECT g.*
		  INTO vZvRow
		  FROM glb_cred_zayav g
		 WHERE g.id = vResRow.Zv_Id;
		IF vResRow.Summa != vZvRow.Zayav_Summa
		THEN
			oMessage := '������ ������� ������ ��������� ���� ����: ' ||
						vResRow.Summa || ' != ' || vZvRow.Zayav_Summa;
		ELSE
			UPDATE qqb_hl_resurs r SET r.state = '3' WHERE r.id = iId_res; --�������� "����������� ��" �� ����������
			prot(iId_res         => vResRow.Id,
				 iIzox           => vResRow.Izox,
				 iState_id       => '2',
				 iOp_id          => NULL,
				 iDate_operation => vDate_operation);
		END IF;
	END confirm;

	FUNCTION is_invalid_date(iId qqb_hl_index_info.id%TYPE) RETURN NUMBER IS
		vMax_date  DATE;
		vOper_date DATE;
		vMaxId     qqb_hl_index_info.id%TYPE;
	BEGIN
		vOper_date := setup.get_operday;
		dbms_output.put_line(USER || ' Tables in the database:');
	
		SELECT t.*
		  INTO vIdxInfRow
		  FROM qqb_hl_index_info t
		 WHERE t.id = iId;
		SELECT COUNT(*)
		  INTO vCount -- berilgan id qatorning 'index_id'doshlari soni.
		  FROM qqb_hl_index_info t
		 WHERE t.index_id = vIdxInfRow.Index_Id;
		IF vCount = 1
		THEN
			-- agar qator yagona bo'lsa shu qatorning 'end_data' ustuni olinadi.
			SELECT t.end_date
			  INTO vMax_date -- 'end_data'ning qiymati 'vMax_date'ga yoziladi.
			  FROM qqb_hl_index_info t
			 WHERE t.index_id = vIdxInfRow.Index_Id;
			IF (vMax_date < vOper_date)
			THEN
				-- agar 'vMax_date' qiymati 'OperDay'dan kichik bo'lsa,
				-- funksiya:
				RETURN 1; -- 1, ya'ni XATO
			ELSE
				RETURN 0; -- 0, ya'ni TO'G'RI qiymatini qaytaradi.
			END IF;
		ELSIF vCount > 1
		THEN
			-- agar qatorlar soni 1 dan ko'p bo'lsa, eng qatorlardagi eng oxirgi 'end_date' ustuni olinadi.
			SELECT MAX(t.end_date)
			  INTO vMax_date
			  FROM qqb_hl_index_info t
			 WHERE t.index_id = vIdxInfRow.Index_Id;
			SELECT t.id
			  INTO vMaxId
			  FROM qqb_hl_index_info t
			 WHERE t.index_id = vIdxInfRow.Index_Id
				   AND t.end_date = vMax_date;
			IF (vMax_date < vOper_date AND vMaxId = iId)
			THEN
				RETURN 1;
			ELSE
				RETURN 0;
			END IF;
		ELSE
			-- (vCount < 1)
			RETURN 0; -- funksiya: 0, ya'ni XATO qiymatini qaytaradi.
		END IF;
	
	END is_invalid_date;

	--//����� ����� ������ ����
	FUNCTION Get_Sch_Name(iSch accounts.acc_external%TYPE) RETURN VARCHAR2 IS
		RESULT         j_array := j_array();
		vCondition     accounts.condition%TYPE;
		vCode_currency accounts.code_currency%TYPE;
		vCurrencyName  REF_CURRENCY.Name%TYPE;
		vMfo           accounts.code_filial%TYPE := '01037';
	BEGIN
		vSchName := bank.Get_Account_Name_any(iSch, 'N', vMfo);
		SELECT t.code_currency
		  INTO vCode_currency
		  FROM accounts t
		 WHERE t.code_filial = vMfo
			   AND t.acc_external = iSch;
		SELECT v.Name
		  INTO vCurrencyName
		  FROM v_currency v
		 WHERE v.CODE = vCode_currency;
		IF vSchName <> '��� ��������'
		THEN
			BEGIN
				SELECT 'X'
				  INTO vCondition
				  FROM accounts
				 WHERE code_filial = vMfo
					   AND acc_external = iSch
					   AND condition = 'A';
			EXCEPTION
				WHEN no_data_found THEN
					vSchName := '���� �� ��������';
				WHEN OTHERS THEN
					vSchName := '����������� ���� �������������';
			END;
		END IF;
		result.push(vSchName);
		result.push(vCode_currency);
		result.push(vCurrencyName);
		RETURN result.to_string();
	END Get_Sch_Name;
	--//
	FUNCTION get_proc_acc
	(
		iMfo accounts.code_filial%TYPE
	   ,iAcc accounts.acc_external%TYPE
	) RETURN VARCHAR2 IS
		RESULT j_array := j_array();
	BEGIN
		SELECT substr(t.account_code, -20)
		  INTO vAcc
		  FROM dep_accounts t
		 WHERE t.account_type = '4'
			   AND t.date_next = '31.12.9999'
			   AND
			   t.contract_id = (SELECT t.contract_id
								  FROM dep_accounts t
								 WHERE substr(t.account_code, -20) = iAcc
									   AND t.account_type = '1'
									   AND (SELECT c.state
											  FROM dep_contracts c
											 WHERE c.id = t.contract_id
												   AND c.filial_code = iMfo) =
									   'APPROVE');
		result.push(vAcc);
		RETURN result.to_string();
	END get_proc_acc;
	--//������� ���������� ������ ����
	FUNCTION Get_Dep_Name(iDep_id dep_contracts.id%TYPE) RETURN VARCHAR2 IS
		RESULT j_array := j_array();
		vName  dep_contracts.contract_name%TYPE;
	BEGIN
		SELECT c.contract_name
		  INTO vName
		  FROM dep_contracts c
		 WHERE c.id = iDep_id;
		result.push(vName);
		RETURN result.to_string();
	END Get_Dep_Name;
	--//������� ���������� ������ ���� 2
	FUNCTION Get_Dep_Name2(iDep_id dep_contracts.id%TYPE) RETURN VARCHAR2 IS
		vName dep_contracts.contract_name%TYPE;
	BEGIN
		SELECT c.contract_name
		  INTO vName
		  FROM dep_contracts c
		 WHERE c.id = iDep_id;
		RETURN vName;
	END Get_Dep_Name2;

	FUNCTION Get_manba_Name
	(
		iZv_id glb_cred_zayav.id%TYPE
	   ,iType  NUMBER DEFAULT 0
	) RETURN VARCHAR2 IS
		vName qqb_hl_spr_manba.name%TYPE;
	BEGIN
		BEGIN
			IF iType = 0
			THEN
				SELECT d.manba
				  INTO vName
				  FROM glb_cred_zayav_desc d
				 WHERE d.id_zv = iZv_id;
			ELSE
				SELECT m.name
				  INTO vName
				  FROM qqb_hl_spr_manba m
				 WHERE m.code = (SELECT d.manba
								   FROM glb_cred_zayav_desc d
								  WHERE d.id_zv = iZv_id);
			END IF;
		EXCEPTION
			WHEN OTHERS THEN
				vName := '';
		END;
		RETURN vName;
	END Get_manba_Name;
	FUNCTION Get_manba_Name1(iCode qqb_hl_spr_manba.code%TYPE) RETURN VARCHAR2 IS
		vName qqb_hl_spr_manba.name%TYPE;
	BEGIN
		BEGIN
			SELECT m.name
			  INTO vName
			  FROM qqb_hl_spr_submanba m
			 WHERE m.id = iCode;
		EXCEPTION
			WHEN OTHERS THEN
				vName := '';
		END;
		RETURN vName;
	END Get_manba_Name1;

	FUNCTION Get_manba_Name2(iCode qqb_hl_spr_manba.code%TYPE) RETURN VARCHAR2 IS
		RESULT j_array := j_array();
		vName  qqb_hl_spr_manba.name%TYPE;
	BEGIN
		SELECT m.name
		  INTO vName
		  FROM qqb_hl_spr_submanba m
		 WHERE m.id = iCode;
		result.push(vName);
		RETURN result.to_string();
	END Get_manba_Name2;

	FUNCTION Get_manba_Name3
	(
		iId   qqb_hl_spr_submanba.id%TYPE
	   ,iDate DATE
	) RETURN VARCHAR2 IS
		RESULT j_array := j_array();
		vProc  qqb_hl_index_info.procent%TYPE;
	BEGIN
		SELECT nvl(t.procent, 0)
		  INTO vProc
		  FROM qqb_hl_index_info t
		 WHERE t.index_id =
			   (SELECT m.type FROM qqb_hl_spr_submanba m WHERE m.id = iId)
			   AND t.begin_date <= iDate;
	EXCEPTION
		WHEN no_data_found THEN
			vProc := 0;
			result.push(vProc);
			RETURN result.to_string();
	END Get_manba_Name3;

	PROCEDURE SendToMain(iId_res qqb_hl_resurs.id%TYPE) IS
	BEGIN
	
		UPDATE qqb_hl_resurs a SET a.state = '1' WHERE a.id = iId_res;
		prot(iId_res, '��������� � ��', 1, 1, SYSDATE);
	
	END;

	-----/----------------/-------------------------/-------------------------------
	FUNCTION chek_schedules_graph(iDep_contract_id dep_contracts.id%TYPE)
		RETURN NUMBER IS
		vDep_id dep_contracts.id%TYPE;
	BEGIN
		BEGIN
			SELECT a.contract_id
			  INTO vDep_id
			  FROM dep_schedules_forecast a
			 WHERE a.contract_id = iDep_contract_id
				   AND rownum = 1;
			IF SQL%FOUND
			THEN
				RETURN NULL;
			END IF;
		EXCEPTION
			WHEN no_data_found THEN
				RETURN 1;
		END;
	END;

	PROCEDURE generator_graph
	(
		iLoan_id                ln_graph_debt.loan_id%TYPE
	   ,iDep_contract_id        dep_contracts.id%TYPE
	   ,IDep_filial_contract_id dep_contracts.id%TYPE
	) IS
	
	BEGIN
		---bosh bank uchun grafik geranatsiya
		IF chek_schedules_graph(iDep_contract_id) IS NOT NULL
		THEN
			FOR rr IN (SELECT *
						 FROM ln_graph_debt g
						WHERE g.loan_id = iLoan_id)
			LOOP
				INSERT INTO dep_schedules_forecast
					(id,
					 contract_id,
					 TYPE,
					 date_validate,
					 amount,
					 emp_code,
					 date_modify)
				VALUES
					(dep_schedules_seq.nextval,
					 iDep_contract_id,
					 1,
					 rr.date_red,
					 rr.summ_red,
					 setup.get_employee_code,
					 SYSDATE);
			
			END LOOP;
		ELSE
			Raise_Application_Error(-20000,
									iDep_contract_id ||
									' Contract id ������� ����������');
		END IF;
		---filial uchun grafik geranatsiya
		IF chek_schedules_graph(IDep_filial_contract_id) IS NOT NULL
		THEN
			FOR rr IN (SELECT *
						 FROM ln_graph_debt g
						WHERE g.loan_id = iLoan_id)
			LOOP
				INSERT INTO dep_schedules_forecast
					(id,
					 contract_id,
					 TYPE,
					 date_validate,
					 amount,
					 emp_code,
					 date_modify)
				VALUES
					(dep_schedules_seq.nextval,
					 IDep_filial_contract_id,
					 1,
					 rr.date_red,
					 rr.summ_red,
					 setup.get_employee_code,
					 SYSDATE);
			
			END LOOP;
		ELSE
			Raise_Application_Error(-20000,
									IDep_filial_contract_id ||
									' Contract id ������� ����������');
		END IF;
	
	END;
	--------------------------------------
	----------------Azizbek---------------
	--------------------------------------
	PROCEDURE contract_action
	(
		i_action_resource       dep_s_action_resource.code%TYPE
	   ,i_client_code           client_current.code%TYPE
	   ,i_contract_name         dep_contracts.contract_name%TYPE
	   ,i_contract_number       dep_contracts.contract_number%TYPE
	   ,i_account_layouts       dep_account_layouts.id%TYPE
	   ,i_deposit_type          dep_s_deposit_type.code%TYPE
	   ,i_contract_date         dep_contracts.contract_date%TYPE
	   ,i_date_begin            dep_contracts.date_begin%TYPE
	   ,i_date_end              dep_contracts.date_end%TYPE
	   ,i_summa                 dep_contracts.summa%TYPE
	   ,i_count_days            dep_contracts.count_days%TYPE
	   ,i_currency_code         dep_contracts.currency_code%TYPE
	   ,i_desc_days_in_year     DEP_s_DAYS_IN_YEAR.Code%TYPE
	   ,i_percent               DEP_CONTRACTS_PERCENT_RATE.PERCENT_RATE%TYPE
	   ,i_percent_DATE_VALIDATE VARCHAR2
	   ,i_Libor_check           VARCHAR2
	   ,o_oid                   OUT NUMBER
	   ,o_error                 OUT VARCHAR2
	) IS
		v_ARRAY_ACCESS      ARRAY_VARCHAR2;
		vtypeCode           VARCHAR2(255);
		vstate              VARCHAR2(255);
		vPERCENT_TYPE_LIST  ARRAY_VARCHAR2 := array_varchar2();
		vDATE_VALIDATE_LIST ARRAY_VARCHAR2 := array_varchar2();
		vPERCENT_RATE_LIST  ARRAY_VARCHAR2 := array_varchar2();
		vLIBOR_CHECK        ARRAY_VARCHAR2 := array_varchar2();
		v_percent           VARCHAR2(20);
	BEGIN
		SELECT decode(substr(to_char(i_percent), 1, 1),
					  '.',
					  '0' || to_char(i_percent),
					  to_char(i_percent))
		  INTO v_percent
		  FROM dual;
	
		dep_api.contract_action(iaction                      => 'ENTER',
								iid                          => NULL,
								OID                          => o_oid,
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
								iswift_code_bank_a           => NULL,
								iswift_code_bank_b           => NULL,
								iswift_from_bank_a           => NULL,
								iswift_from_bank_b           => NULL,
								iswift_to_bank_a             => NULL,
								iswift_to_bank_b             => NULL,
								iswift_proc_from_account     => NULL,
								iswift_proc_to_account       => NULL,
								iswift_related_reference     => NULL,
								iswift_receiver_information  => NULL,
								i_parent_contract_id         => NULL,
								i_pledge                     => 'Y',
								i_product_select             => NULL,
								i_min_balance_operation      => 0,
								i_credit_source_code         => NULL,
								i_foreign_organization_code  => NULL,
								i_financing_currency_code    => NULL,
								i_financing_amount           => NULL,
								i_issuer                     => NULL,
								i_denomination               => NULL,
								i_release_series             => NULL,
								i_interest_frequenc          => NULL,
								i_mm_id                      => NULL,
								i_remark_line0               => NULL,
								i_remark_line1               => NULL,
								i_bloomberg_reuters          => NULL,
								iswift_from_bank_acc         => NULL,
								iswift_to_bank_acc           => NULL,
								iswift_56_a                  => NULL,
								iswift_56_acc                => NULL,
								iswift_to_bank_b_acc         => NULL,
								iswift_receiver_inf72        => NULL,
								iswift_credit_or_debit_c     => NULL,
								iswift_credit_or_debit_acc_c => NULL,
								IEMP_RESOURCE_CODE           => NULL,
								IBUDGET_ACC_OSN              => NULL,
								IBUDGET_ACC_PERC             => NULL,
								IIS_ANNUITET                 => NULL,
								iuser_id                     => NULL);
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
	
		IF upper(v_ARRAY_ACCESS(1)) = 'Y'
		THEN
			SELECT dep_api.get_obj_contract INTO vtypeCode FROM dual;
			dep_api.initialize_object(iObject_Id        => o_oid,
									  iObject_Type_Code => vtypeCode);
			SELECT dep_api.get_contract_param(ikey => 'STATE')
			  INTO vstate
			  FROM dual;
		END IF;
	EXCEPTION
		WHEN OTHERS THEN
			o_error := SQLERRM;
	END contract_action;
	----���������: Dep_jur
	PROCEDURE contract_action_approve
	(
		IID_LIST IN ARRAY_VARCHAR2
	   ,OMESSAGE OUT VARCHAR2
	) IS
	BEGIN
		dep_api.contract_action(OMESSAGE => OMESSAGE,
								IACTION  => 'APPROVE',
								IID_LIST => IID_LIST);
	END contract_action_approve;

	PROCEDURE accept
	(
		i_submanba_id           NUMBER
	   ,i_contract_name         dep_contracts.contract_name%TYPE
	   ,i_contract_number       dep_contracts.contract_number%TYPE
	   ,i_contract_date         VARCHAR2
	   ,i_date_begin            VARCHAR2
	   ,i_date_end              VARCHAR2
	   ,i_percent               DEP_CONTRACTS_PERCENT_RATE.PERCENT_RATE%TYPE
	   ,i_percent_DATE_VALIDATE VARCHAR2
	   ,i_desc_days_in_year     DEP_s_DAYS_IN_YEAR.Code%TYPE
	   ,i_zv_id                 NUMBER
	   ,i_resurs_id             qqb_hl_resurs.id%TYPE
	   ,o_Message               OUT VARCHAR2
	) IS
	
		v_dep_id_b        INTEGER;
		v_dep_id_f        INTEGER;
		v_error           VARCHAR2(32767);
		v_action_resource dep_s_action_resource.code%TYPE;
		v_account_layouts dep_account_layouts.id%TYPE; --������ ������; ������. ������� 16102-840
		v_deposit_type    dep_s_deposit_type.code%TYPE; --���������� ����� �������; ������������� �������
		v_contract_date   dep_contracts.contract_date%TYPE := to_date(i_contract_date,
																	  'dd.mm.yyyy');
		v_date_end        dep_contracts.date_end%TYPE;
		v_currency_code   dep_contracts.currency_code%TYPE;
	
		v_summa      dep_contracts.summa%TYPE;
		v_cl_code    dep_contracts.client_code%TYPE;
		v_loan_id    ln_card.loan_id%TYPE;
		v_count_days dep_contracts.count_days%TYPE;
		v_date_begin ln_card.open_date%TYPE;
	
		vEmpCode       core_users.user_id%TYPE; --current
		vEmpHeaderCode core_users.user_id%TYPE; --bosh bank
		vEmpZvCode     core_users.user_id%TYPE; --zayavka
		v_mfo          CHAR(5);
		v_currency     qqb_hl_spr_submanba.currency%TYPE;
		v_Libor_check  VARCHAR2(1) := 'N';
		-----------------------------------------------------------
		boshMfo       VARCHAR2(5) := '01037';
		v_acc_res_bo  accounts.acc_external%TYPE;
		v_acc_res_fil accounts.acc_external%TYPE;
		v_acc_code    accounts.code%TYPE;
		f_acc_code    accounts.code%TYPE;
	
	BEGIN
		BEGIN
			SELECT t.currency
			  INTO v_currency
			  FROM qqb_hl_spr_submanba t
			 WHERE id = i_submanba_id;
			SELECT t.cl_code, t.loan_id, t.mfo
			  INTO v_cl_code, v_loan_id, v_mfo
			  FROM glb_cred_zayav t
			 WHERE id = i_zv_id;
			-----------------------------------------------------------
			SELECT t.acc_res_bo, t.acc_res_fil
			  INTO v_acc_res_bo, v_acc_res_fil
			  FROM QQB_HL_ACCOUNTS t
			 WHERE t.filial = v_mfo
				   AND t.t2_id = i_submanba_id;
			SELECT a.code
			  INTO v_acc_code
			  FROM accounts a
			 WHERE a.code_filial = boshMfo
				   AND a.acc_external = v_acc_res_bo;
			SELECT a.code
			  INTO f_acc_code
			  FROM accounts a
			 WHERE a.code_filial = v_mfo
				   AND a.acc_external = v_acc_res_fil;
			------------------------------------------------------
			SELECT da.contract_id
			  INTO v_dep_id_b
			  FROM dep_accounts da
			 WHERE da.account_code = v_acc_code
				   AND EXISTS (SELECT *
					  FROM dep_contracts a
					 WHERE a.id = da.contract_id
						   AND a.state = 'APPROVE')
				   AND rownum = 1;
			SELECT da.contract_id
			  INTO v_dep_id_f
			  FROM dep_accounts da
			 WHERE da.account_code = f_acc_code
				   AND EXISTS (SELECT *
					  FROM dep_contracts a
					 WHERE a.id = da.contract_id
						   AND a.state = 'APPROVE')
				   AND rownum = 1;
		
			IF SQL%FOUND
			THEN
				UPDATE qqb_hl_resurs t
				   SET t.submanba_id = i_submanba_id,
					   t.dep_id_b    = v_dep_id_b,
					   t.dep_id_f    = v_dep_id_f,
					   t.state       = 2
				 WHERE t.id = i_resurs_id;
			END IF;
		EXCEPTION
			WHEN no_data_found THEN
				BEGIN
					IF v_cl_code IS NULL
					THEN
						o_Message := '������ ���������� ����� ������ ���� ������������?!';
						Raise_Application_Error(-20000, o_Message);
					END IF;
					IF v_loan_id IS NULL
					THEN
						o_Message := '������ ���������� loan_id ���������������?!';
						Raise_Application_Error(-20000, o_Message);
					END IF;
					vEmpCode := setup.get_employee_code();
					SELECT a.user_id
					  INTO vEmpHeaderCode
					  FROM core_users a
					 WHERE a.filial_code = '01037'
						   AND a.private_post_id = 4
						   AND a.state = 'A';
					SELECT a.user_id
					  INTO vEmpZvCode
					  FROM core_users a
					 WHERE a.filial_code = v_mfo
						   AND a.private_post_id = 4
						   AND a.state = 'A';
				
					IF v_loan_id IS NOT NULL
					THEN
						SELECT nvl(t.summ_loan, 0) / 100,
							   t.currency,
							   t.open_date,
							   t.close_date
						  INTO v_summa,
							   v_currency_code,
							   v_date_begin,
							   v_date_end
						  FROM ln_card t
						 WHERE loan_id = v_loan_id;
						IF i_date_begin IS NOT NULL
						THEN
							v_date_begin := to_date(i_date_begin,
													'dd.mm.yyyy');
						END IF;
						IF i_date_end IS NOT NULL
						THEN
							v_date_end := to_date(i_date_end, 'dd.mm.yyyy');
						END IF;
						SELECT v_date_end - v_date_begin
						  INTO v_count_days
						  FROM dual;
						-- ������ Libor ������� ����� �������������
						-- ������ Libor ������� ������ ����������� ������� �����
						/*        fozil_actions.ln_percent_update(iLoanId     => v_loan_id,
                                                                iTypeAction => 'NEW' );
                        */
					
						----Shartnoma; ����������
						setup.set_employee_code(vEmpHeaderCode);
						v_action_resource := 'PLACEMENT';
						IF v_currency = '978'
						THEN
							v_account_layouts := 74; --������ ������; ������. ������� 16102-978
						END IF;
						IF v_currency = '840'
						THEN
							v_account_layouts := 63; --������ ������; ������. ������� 16102-840
						END IF;
					
						v_deposit_type := 4; --���������� ����� �������; ������������� �������
						contract_action(i_action_resource       => v_action_resource,
										i_client_code           => lpad(v_mfo,
																		8,
																		'0'),
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
						IF v_error IS NOT NULL
						THEN
							ROLLBACK;
							o_Message := v_error;
							Raise_Application_Error(-20000, v_error);
						ELSE
							----Shartnoma; �����������
							setup.set_employee_code(vEmpZvCode);
							v_action_resource := 'ATTRACT';
							IF v_currency = '978'
							THEN
								v_account_layouts := 76; --������ ������; ������. ������� 16102-978
							END IF;
							IF v_currency = '840'
							THEN
								v_account_layouts := 62; --������ ������; ������. ������� 16102-840
							END IF;
							v_deposit_type := 4; --���������� ����� �������; ������������� �������
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
						
							IF v_error IS NOT NULL
							THEN
								o_Message := v_error;
								ROLLBACK;
								Raise_Application_Error(-20000, v_error);
							ELSE
								---Shaxzod qo'shgan grafik genratsiya qiladi
								generator_graph(iLoan_id                => v_Loan_id,
												iDep_contract_id        => v_dep_id_b,
												IDep_filial_contract_id => v_dep_id_f);
							
								---��� �� ��� ����������
								UPDATE qqb_hl_resurs t
								   SET t.submanba_id = i_submanba_id,
									   t.dep_id_b    = v_dep_id_b,
									   t.dep_id_f    = v_dep_id_f,
									   t.state       = 2
								 WHERE t.id = i_resurs_id;
								--Nazarov X.A 28.09.2020
								--- ��������� ����� �����
								--�������� ����
								INSERT INTO dep_contracts_mode_actions
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
								VALUES
									(v_dep_id_b,
									 'ACD',
									 'MANUAL',
									 'N',
									 'N',
									 vEmpHeaderCode,
									 SYSDATE,
									 'Y',
									 'USUAL',
									 'N');
								INSERT INTO dep_contracts_mode_actions_his
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
								VALUES
									(v_dep_id_b,
									 'ACD',
									 'MANUAL',
									 'N',
									 'N',
									 vEmpHeaderCode,
									 SYSDATE,
									 'Y',
									 'USUAL',
									 'N');
								--������
								INSERT INTO dep_contracts_mode_actions
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
								VALUES
									(v_dep_id_f,
									 'ACD',
									 'MANUAL',
									 'N',
									 'N',
									 vEmpZvCode,
									 SYSDATE,
									 'Y',
									 'USUAL',
									 'N');
								INSERT INTO dep_contracts_mode_actions_his
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
								VALUES
									(v_dep_id_f,
									 'ACD',
									 'MANUAL',
									 'N',
									 'N',
									 vEmpZvCode,
									 SYSDATE,
									 'Y',
									 'USUAL',
									 'N');
								--������� ����������. ��� ����� ���� ����������
								openBindAcc(v_mfo,
											i_submanba_id,
											v_dep_id_b,
											v_dep_id_f,
											o_Message);
								-- Sherzodaka yozgan protokolni chaqirilgan
								qqb_resurs_val.prot(i_resurs_id,
													'������ �������',
													2,
													3,
													SYSDATE);
								--o_Message := 'Ma`lumotlar muvaffaqiyatli saqlandi';
							END IF;
						END IF;
					ELSE
						o_Message := '����� ������ ����������� ������������!!!';
						Raise_Application_Error(-20000,
												'����� ������ ����������� ������������!!!');
					END IF;
					setup.set_employee_code(vEmpCode);
				EXCEPTION
					WHEN OTHERS THEN
						setup.set_employee_code(vEmpCode);
						o_Message := SQLERRM;
						Raise_Application_Error(-20000, SQLERRM);
				END;
		END;
	END accept;

	PROCEDURE bindAccount
	(
		iAccount_code  VARCHAR2
	   ,iFilialCode    VARCHAR2
	   ,iContractId    VARCHAR2
	   ,iResultMessage OUT VARCHAR2
	) IS
		vAccountTypeCode array_varchar2;
		vFilialCode      array_varchar2;
		vAccount_code    array_varchar2;
		vSubCoa          array_varchar2;
		vDateValidate    array_varchar2;
	BEGIN
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
	END;
	----- Bitta hisob raqam ochish
	PROCEDURE createAccount
	(
		iCreatorFilial VARCHAR2
	   ,iBindFilial    VARCHAR2
	   ,iTemplate      VARCHAR2
	   ,iClentName     VARCHAR2
	   ,iOrdering      VARCHAR2 DEFAULT '310'
	   ,oCondition     OUT VARCHAR2
	   ,oDialog        OUT VARCHAR2
	   ,oMessage       OUT VARCHAR2
	) IS
		oAccountCode   ACCOUNTS.CODE%TYPE;
		vEmpCode       core_users.user_id%TYPE;
		iOperation     VARCHAR2(1) := 'A';
		vFilialEmpCode core_users.user_id%TYPE;
		oBindMessage   VARCHAR2(5000);
		vClUId         client_current.client_uid%TYPE;
	
	BEGIN
		BEGIN
			vEmpCode := setup.get_employee_code;
			SELECT a.user_id
			  INTO vFilialEmpCode
			  FROM core_users a
			 WHERE a.filial_code = iCreatorFilial
				   AND a.private_post_id = 4
				   AND a.state = 'A';
			--vFilialEmpCode:=setup.get_employee_code;
			setup.set_employee_code(vFilialEmpCode);
			oAccountCode := '11' || setup.get_local_code || iTemplate ||
							'000' || iBindFilial || iOrdering;
			SELECT substr(oAccountCode, 17, 8) INTO vClUId FROM dual;
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
			WHEN OTHERS THEN
				setup.set_employee_code(vEmpCode);
				oMessage := SQLERRM;
		END;
	END;

	PROCEDURE createAccounts
	(
		iT2_ID      VARCHAR2
	   ,iFilial     VARCHAR2
	   ,iClientName VARCHAR2
	   ,oCondition  OUT VARCHAR2
	   ,oDialog     OUT VARCHAR2
	   ,oMessage    OUT VARCHAR2
	) IS
		vRow       qqb_hl_accounts%ROWTYPE;
		vTemplate  VARCHAR2(30);
		vCondition VARCHAR2(40) := '';
		vDialog    VARCHAR2(32000) := '';
		vMessage   VARCHAR2(32000) := '';
		vOrdering  VARCHAR2(3) := '310';
	
		vBO_res_schet   VARCHAR2(30);
		vFIL_res_schet  VARCHAR2(30);
		vBO_foiz_schet  VARCHAR2(30);
		vFIL_foiz_schet VARCHAR2(30);
	BEGIN
		FOR vRow IN (SELECT *
					   FROM qqb_hl_accounts acc
					  WHERE acc.filial = iFilial
							AND acc.t2_id = iT2_ID)
		LOOP
			vTemplate := substr(vRow.Acc_Res_Bo, 1, 9);
			IF substr(vRow.Acc_Res_Bo, 9, 1) = '_'
			THEN
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
			END IF;
		
			vTemplate := substr(vRow.Acc_Foiz_Bo, 1, 9);
			IF substr(vTemplate, 9, 1) = '_'
			THEN
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
			END IF;
		
			vTemplate := substr(vRow.Acc_Res_Fil, 1, 9);
			IF substr(vTemplate, 9, 1) = '_'
			THEN
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
			END IF;
		
			vTemplate := substr(vRow.Acc_Foiz_Fil, 1, 9);
			IF substr(vTemplate, 9, 1) = '_'
			THEN
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
			END IF;
		
			UPDATE qqb_hl_accounts t
			   SET t.acc_res_bo   = substr(vBO_res_schet, 8),
				   t.acc_foiz_bo  = substr(vBO_foiz_schet, 8),
				   t.acc_res_fil  = substr(vfil_res_schet, 8),
				   t.acc_foiz_fil = substr(vfil_foiz_schet, 8)
			 WHERE t.t2_id = iT2_ID
				   AND t.filial = iFilial
				   AND length(vBO_res_schet) = 27
				   AND length(vBO_foiz_schet) = 27
				   AND length(vfil_res_schet) = 27
				   AND length(vfil_foiz_schet) = 27;
		END LOOP;
	END;
	--Nazarox X.A  28.09.2020 h/r biriktirish
	PROCEDURE openBindAcc
	(
		iMfo        VARCHAR2
	   ,iSubmanbaId INTEGER
	   ,b_id        dep_contracts.id%TYPE
	   ,f_id        dep_contracts.id%TYPE
	   ,oMessage    OUT VARCHAR2
	) IS
		v_acc_res_bo    accounts.acc_external%TYPE;
		v_acc_foiz_bo   accounts.acc_external%TYPE;
		v_acc_res_fil   accounts.acc_external%TYPE;
		v_acc_foiz_fil  accounts.acc_external%TYPE;
		v_acc_code      accounts.code%TYPE;
		f_acc_code      accounts.code%TYPE;
		v_acc_external  accounts.acc_external%TYPE;
		v_acc_condition accounts.condition%TYPE;
		vCode_b         v_emp_pass.code%TYPE;
		vCode_f         v_emp_pass.code%TYPE;
		iCondition      client_current.condition%TYPE;
		vClUId          client_current.client_uid%TYPE;
		filial_unqcode  accounts.client_code%TYPE;
		bosh_unqcode    accounts.client_code%TYPE := '00001037';
		account_name    accounts.name%TYPE;
		tartibNo        CHAR(3);
		oAccountCode    ACCOUNTS.CODE%TYPE;
		iOperation      S_ACC_OPERATION.OPERATION%TYPE;
		oCondition      S_ACC_OPERATION.CONDITION_RESULT%TYPE;
		oDialog         S_ACC_OPERATION.DIALOG_NEED%TYPE;
		boshMfo         VARCHAR2(5) := '01037';
		v_id            accounts.id%TYPE;
		v_code_coa      accounts.code_coa%TYPE;
		v_foiz_code     accounts.code%TYPE;
		f_foiz_code     accounts.code%TYPE;
		--���� �����
		vCurrCode     qqb_hl_spr_submanba.currency%TYPE;
		vAccId_6      accounts.id%TYPE;
		vAccId_11     accounts.id%TYPE;
		vAccId_12     accounts.id%TYPE;
		f_acc_code_5  accounts.code%TYPE;
		f_acc_code_11 accounts.code%TYPE;
		f_acc_code_12 accounts.code%TYPE;
	BEGIN
		SELECT p.code
		  INTO vCode_b
		  FROM v_emp_pass p
		 WHERE p.filial_code = boshMfo
			   AND p.rank_code = 4
			   AND p.condition = 'A';
		SELECT p.code
		  INTO vCode_f
		  FROM v_emp_pass p
		 WHERE p.filial_code = iMfo
			   AND p.rank_code = 4
			   AND p.condition = 'A';
		filial_unqcode := lpad(iMfo, 8, '0');
		BEGIN
			SELECT t.acc_res_bo,
				   t.acc_foiz_bo,
				   t.acc_res_fil,
				   t.acc_foiz_fil
			  INTO v_acc_res_bo,
				   v_acc_foiz_bo,
				   v_acc_res_fil,
				   v_acc_foiz_fil
			  FROM QQB_HL_ACCOUNTS t
			 WHERE t.filial = iMfo
				   AND t.t2_id = iSubmanbaId;
			IF SQL%FOUND
			THEN
				SELECT c.condition
				  INTO iCondition
				  FROM client_current c
				 WHERE c.code = filial_unqcode;
				IF iCondition = 'T'
				THEN
					UPDATE client_current t
					   SET t.condition             = 'A',
						   t.date_validate         = SYSDATE,
						   t.date_change_condition = setup.get_operday
					 WHERE t.code = filial_unqcode;
					INSERT INTO client_history
						SELECT *
						  FROM client_current
						 WHERE code = filial_unqcode;
				END IF;
				SELECT c.condition
				  INTO iCondition
				  FROM client_current c
				 WHERE c.code = bosh_unqcode;
				IF iCondition = 'T'
				THEN
					UPDATE client_current t
					   SET t.condition             = 'A',
						   t.date_validate         = SYSDATE,
						   t.date_change_condition = setup.get_operday
					 WHERE t.code = bosh_unqcode;
					INSERT INTO client_history
						SELECT *
						  FROM client_current
						 WHERE code = bosh_unqcode;
				END IF;
			
			END IF;
		EXCEPTION
			WHEN no_data_found THEN
				oMessage := 'QQB_HL_ACCOUNTS ��������� ������ ���������?!';
				RETURN;
		END;
		--Bosh bankda asosiy schet(16102)
		BEGIN
			SELECT a.code, a.condition, a.id, a.code_coa
			  INTO v_acc_code, v_acc_condition, v_id, v_code_coa
			  FROM accounts a
			 WHERE a.code_filial = boshMfo
				   AND a.acc_external = v_acc_res_bo;
		EXCEPTION
			WHEN no_data_found THEN
				--01037 da 16102 ochish;
				setup.Set_Employee_Code(vCode_b);
				--tartib nomer
				IF substr(v_acc_res_bo, -3) = '___'
				THEN
					BEGIN
						SELECT lpad(MAX(substr(a.acc_external, -3)) + 1,
									3,
									'0')
						  INTO tartibNo
						  FROM accounts a
						 WHERE a.code_filial = boshMfo
							   AND a.acc_external LIKE
							   substr(v_acc_res_bo, 1, 9) ||
							   filial_unqcode || '___';
					EXCEPTION
						WHEN no_data_found THEN
							tartibNo := '001';
					END;
				ELSE
					tartibNo := substr(v_acc_res_bo, -3);
				END IF;
				--schet nomi
				SELECT t.name || ' ������ ������� ������ ' || b.name ||
					   '�� �������� ���������'
				  INTO account_name
				  FROM QQB_HL_SPR_SUBMANBA t, bank_desc_glb b
				 WHERE t.id = iSubmanbaId
					   AND b.code = iMfo;
				--
				oAccountCode := '11' || setup.get_local_code ||
								substr(v_acc_res_bo, 1, 9) ||
								filial_unqcode || tartibNo;
				BEGIN
					SELECT a.code
					  INTO v_acc_code
					  FROM accounts a
					 WHERE a.code LIKE oAccountCode;
				EXCEPTION
					WHEN no_data_found THEN
						SELECT c.client_uid
						  INTO vClUId
						  FROM client_current c
						 WHERE c.code = filial_unqcode
							   AND rownum = 1;
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
					
				END;
		END;
		--��� �� qqb_hl_accounts�� ����������
		BEGIN
			SELECT a.code, a.acc_external, a.id, a.code_coa
			  INTO v_acc_code, v_acc_external, v_id, v_code_coa
			  FROM accounts a
			 WHERE a.code = v_acc_code;
			IF SQL%FOUND
			THEN
				UPDATE dep_accounts
				   SET date_next = setup.get_operday
				 WHERE contract_id = b_id
					   AND account_type = 1
					   AND date_next = to_date('31.12.9999', 'dd.mm.yyyy');
				INSERT INTO dep_accounts
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
				VALUES
					(b_id,
					 1,
					 setup.get_operday,
					 boshMfo,
					 v_acc_code,
					 vCode_b,
					 SYSDATE,
					 to_date('31.12.9999', 'dd.mm.yyyy'),
					 v_id,
					 v_code_coa);
				INSERT INTO dep_accounts_his
					(contract_id,
					 account_type,
					 date_validate,
					 filial_code,
					 account_code,
					 emp_code,
					 date_modify,
					 action)
				VALUES
					(b_id,
					 1,
					 setup.get_operday,
					 boshMfo,
					 v_acc_code,
					 vCode_b,
					 SYSDATE,
					 'I');
				UPDATE qqb_hl_accounts h
				   SET h.acc_res_bo = v_acc_external
				 WHERE h.t2_id = iSubmanbaId
					   AND h.filial = iMfo;
			END IF;
		EXCEPTION
			WHEN OTHERS THEN
				NULL;
		END;
		--Bosh bankda procent schet(16304)
		BEGIN
			SELECT a.code, a.condition, a.id, a.code_coa
			  INTO v_foiz_code, v_acc_condition, v_id, v_code_coa
			  FROM accounts a
			 WHERE a.code_filial = boshMfo
				   AND a.acc_external = v_acc_foiz_bo;
		EXCEPTION
			WHEN no_data_found THEN
				--01037 da 16304 ochish;
				setup.Set_Employee_Code(vCode_b);
				--tartib nomer
				IF substr(v_acc_foiz_bo, -3) = '___'
				THEN
					BEGIN
						SELECT lpad(MAX(substr(a.acc_external, -3)) + 1,
									3,
									'0')
						  INTO tartibNo
						  FROM accounts a
						 WHERE a.code_filial = boshMfo
							   AND a.acc_external LIKE
							   substr(v_acc_foiz_bo, 1, 9) ||
							   filial_unqcode || '___';
					EXCEPTION
						WHEN no_data_found THEN
							tartibNo := '001';
					END;
				ELSE
					tartibNo := substr(v_acc_foiz_bo, -3);
				END IF;
				--schet nomi
				SELECT t.name || ' ������ ������� ������ ' || b.name ||
					   '�� �������� �������� �����.�������'
				  INTO account_name
				  FROM QQB_HL_SPR_SUBMANBA t, bank_desc_glb b
				 WHERE t.id = iSubmanbaId
					   AND b.code = iMfo;
				--
				oAccountCode := '11' || setup.get_local_code ||
								substr(v_acc_foiz_bo, 1, 9) ||
								filial_unqcode || tartibNo;
				BEGIN
					SELECT a.code
					  INTO v_foiz_code
					  FROM accounts a
					 WHERE a.code LIKE oAccountCode;
				EXCEPTION
					WHEN no_data_found THEN
						SELECT c.client_uid
						  INTO vClUId
						  FROM client_current c
						 WHERE c.code = filial_unqcode
							   AND rownum = 1;
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
					
				END;
		END;
		--��� �� qqb_hl_accounts�� ����������
		BEGIN
			SELECT a.code, a.acc_external, a.id, a.code_coa
			  INTO v_foiz_code, v_acc_external, v_id, v_code_coa
			  FROM accounts a
			 WHERE a.code = v_foiz_code;
			IF SQL%FOUND
			THEN
				UPDATE dep_accounts
				   SET date_next = setup.get_operday
				 WHERE contract_id = b_id
					   AND account_type = 4
					   AND date_next = to_date('31.12.9999', 'dd.mm.yyyy');
				INSERT INTO dep_accounts
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
				VALUES
					(b_id,
					 4,
					 setup.get_operday,
					 boshMfo,
					 v_foiz_code,
					 vCode_b,
					 SYSDATE,
					 to_date('31.12.9999', 'dd.mm.yyyy'),
					 v_id,
					 v_code_coa);
				INSERT INTO dep_accounts_his
					(contract_id,
					 account_type,
					 date_validate,
					 filial_code,
					 account_code,
					 emp_code,
					 date_modify,
					 action)
				VALUES
					(b_id,
					 4,
					 setup.get_operday,
					 boshMfo,
					 v_foiz_code,
					 vCode_b,
					 SYSDATE,
					 'I');
				UPDATE qqb_hl_accounts h
				   SET h.acc_foiz_bo = v_acc_external
				 WHERE h.t2_id = iSubmanbaId
					   AND h.filial = iMfo;
			END IF;
			--shu erga 44905,17101840,17101 larni ham biriktir
		EXCEPTION
			WHEN OTHERS THEN
				NULL;
		END;
		--��� ������ ��� �� 6, 11, 12 - ��� ��������� ����������
		SELECT s.currency
		  INTO vCurrCode
		  FROM QQB_HL_SPR_SUBMANBA s
		 WHERE s.id = iSubmanbaId;
		IF vCurrCode = '840'
		THEN
			vAccId_6  := '8101868';
			vAccId_11 := '2532784';
			vAccId_12 := '2094801';
		ELSIF vCurrCode = '978'
		THEN
			vAccId_6  := '8101868';
			vAccId_11 := '8345511';
			vAccId_12 := '8345510';
		END IF;
		-- 6-��� ����
		BEGIN
			SELECT a.code, a.acc_external, a.id, a.code_coa
			  INTO v_acc_code, v_acc_external, v_id, v_code_coa
			  FROM accounts a
			 WHERE a.id = vAccId_6;
			IF SQL%FOUND
			THEN
				UPDATE dep_accounts
				   SET date_next = setup.get_operday
				 WHERE contract_id = b_id
					   AND account_type = 6
					   AND date_next = to_date('31.12.9999', 'dd.mm.yyyy');
				INSERT INTO dep_accounts
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
				VALUES
					(b_id,
					 6,
					 setup.get_operday,
					 boshMfo,
					 v_acc_code,
					 vCode_b,
					 SYSDATE,
					 to_date('31.12.9999', 'dd.mm.yyyy'),
					 v_id,
					 v_code_coa);
				INSERT INTO dep_accounts_his
					(contract_id,
					 account_type,
					 date_validate,
					 filial_code,
					 account_code,
					 emp_code,
					 date_modify,
					 action)
				VALUES
					(b_id,
					 6,
					 setup.get_operday,
					 boshMfo,
					 v_acc_code,
					 vCode_b,
					 SYSDATE,
					 'I');
			END IF;
		EXCEPTION
			WHEN OTHERS THEN
				NULL;
		END;
		-- 11-��� ����
		BEGIN
			SELECT a.code, a.acc_external, a.id, a.code_coa
			  INTO v_acc_code, v_acc_external, v_id, v_code_coa
			  FROM accounts a
			 WHERE a.id = vAccId_11;
			IF SQL%FOUND
			THEN
				UPDATE dep_accounts
				   SET date_next = setup.get_operday
				 WHERE contract_id = b_id
					   AND account_type = 11
					   AND date_next = to_date('31.12.9999', 'dd.mm.yyyy');
				INSERT INTO dep_accounts
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
				VALUES
					(b_id,
					 11,
					 setup.get_operday,
					 boshMfo,
					 v_acc_code,
					 vCode_b,
					 SYSDATE,
					 to_date('31.12.9999', 'dd.mm.yyyy'),
					 v_id,
					 v_code_coa);
				INSERT INTO dep_accounts_his
					(contract_id,
					 account_type,
					 date_validate,
					 filial_code,
					 account_code,
					 emp_code,
					 date_modify,
					 action)
				VALUES
					(b_id,
					 11,
					 setup.get_operday,
					 boshMfo,
					 v_acc_code,
					 vCode_b,
					 SYSDATE,
					 'I');
			END IF;
		EXCEPTION
			WHEN OTHERS THEN
				NULL;
		END;
		-- 12-��� ����
		BEGIN
			SELECT a.code, a.acc_external, a.id, a.code_coa
			  INTO v_acc_code, v_acc_external, v_id, v_code_coa
			  FROM accounts a
			 WHERE a.id = vAccId_12;
			IF SQL%FOUND
			THEN
				UPDATE dep_accounts
				   SET date_next = setup.get_operday
				 WHERE contract_id = b_id
					   AND account_type = 12
					   AND date_next = to_date('31.12.9999', 'dd.mm.yyyy');
				INSERT INTO dep_accounts
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
				VALUES
					(b_id,
					 12,
					 setup.get_operday,
					 boshMfo,
					 v_acc_code,
					 vCode_b,
					 SYSDATE,
					 to_date('31.12.9999', 'dd.mm.yyyy'),
					 v_id,
					 v_code_coa);
				INSERT INTO dep_accounts_his
					(contract_id,
					 account_type,
					 date_validate,
					 filial_code,
					 account_code,
					 emp_code,
					 date_modify,
					 action)
				VALUES
					(b_id,
					 12,
					 setup.get_operday,
					 boshMfo,
					 v_acc_code,
					 vCode_b,
					 SYSDATE,
					 'I');
			END IF;
		EXCEPTION
			WHEN OTHERS THEN
				NULL;
		END;
		--Filialda asosiy schet(22203)
		BEGIN
			SELECT a.code, a.condition, a.id, a.code_coa
			  INTO f_acc_code, v_acc_condition, v_id, v_code_coa
			  FROM accounts a
			 WHERE a.code_filial = iMfo
				   AND a.acc_external = v_acc_res_fil;
		EXCEPTION
			WHEN no_data_found THEN
				--�������� da 22203 ochish;
				setup.Set_Employee_Code(vCode_f);
				--tartib nomer
				IF substr(v_acc_res_fil, -3) = '___'
				THEN
					BEGIN
						SELECT lpad(MAX(substr(a.acc_external, -3)) + 1,
									3,
									'0')
						  INTO tartibNo
						  FROM accounts a
						 WHERE a.code_filial = iMfo
							   AND a.acc_external LIKE
							   substr(v_acc_res_fil, 1, 9) ||
							   bosh_unqcode || '___';
					EXCEPTION
						WHEN no_data_found THEN
							tartibNo := '001';
					END;
				ELSE
					tartibNo := substr(v_acc_res_fil, -3);
				END IF;
				--schet nomi
				SELECT t.name ||
					   ' ������ ������� ������ ��� ������� ������� ���������'
				  INTO account_name
				  FROM QQB_HL_SPR_SUBMANBA t
				 WHERE t.id = iSubmanbaId;
				--
				oAccountCode := '11' || setup.get_local_code ||
								substr(v_acc_res_fil, 1, 9) ||
								bosh_unqcode || tartibNo;
				BEGIN
					SELECT a.code
					  INTO f_acc_code
					  FROM accounts a
					 WHERE a.code LIKE oAccountCode;
				EXCEPTION
					WHEN no_data_found THEN
						SELECT c.client_uid
						  INTO vClUId
						  FROM client_current c
						 WHERE c.code = filial_unqcode
							   AND rownum = 1;
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
					
				END;
		END;
		--��� �� qqb_hl_accounts�� ����������
		BEGIN
			SELECT a.code, a.acc_external, a.id, a.code_coa
			  INTO f_acc_code, v_acc_external, v_id, v_code_coa
			  FROM accounts a
			 WHERE a.code = f_acc_code;
			IF SQL%FOUND
			THEN
				UPDATE dep_accounts
				   SET date_next = setup.get_operday
				 WHERE contract_id = f_id
					   AND account_type = 1
					   AND date_next = to_date('31.12.9999', 'dd.mm.yyyy');
				INSERT INTO dep_accounts
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
				VALUES
					(f_id,
					 1,
					 setup.get_operday,
					 iMfo,
					 f_acc_code,
					 vCode_b,
					 SYSDATE,
					 to_date('31.12.9999', 'dd.mm.yyyy'),
					 v_id,
					 v_code_coa);
				INSERT INTO dep_accounts_his
					(contract_id,
					 account_type,
					 date_validate,
					 filial_code,
					 account_code,
					 emp_code,
					 date_modify,
					 action)
				VALUES
					(f_id,
					 1,
					 setup.get_operday,
					 iMfo,
					 f_acc_code,
					 vCode_b,
					 SYSDATE,
					 'I');
				UPDATE qqb_hl_accounts h
				   SET h.acc_res_fil = v_acc_external
				 WHERE h.t2_id = iSubmanbaId
					   AND h.filial = iMfo;
				v_acc_res_fil := v_acc_external;
			END IF;
		EXCEPTION
			WHEN OTHERS THEN
				NULL;
		END;
		--Filialda procent schet(22409)
		BEGIN
			SELECT a.code, a.condition, a.id, a.code_coa
			  INTO f_foiz_code, v_acc_condition, v_id, v_code_coa
			  FROM accounts a
			 WHERE a.code_filial = iMfo
				   AND a.acc_external = v_acc_foiz_fil;
		EXCEPTION
			WHEN no_data_found THEN
				--��������  22409 ochish;
				setup.Set_Employee_Code(vCode_f);
				--tartib nomer
				IF substr(v_acc_foiz_fil, -3) = '___'
				THEN
					BEGIN
						SELECT lpad(MAX(substr(a.acc_external, -3)) + 1,
									3,
									'0')
						  INTO tartibNo
						  FROM accounts a
						 WHERE a.code_filial = iMfo
							   AND a.acc_external LIKE
							   substr(v_acc_foiz_fil, 1, 9) ||
							   bosh_unqcode || '___';
					EXCEPTION
						WHEN no_data_found THEN
							tartibNo := '001';
					END;
				ELSE
					tartibNo := substr(v_acc_foiz_fil, -3);
				END IF;
				--schet nomi
				SELECT t.name || ' ������ ������� ������ ' || b.name ||
					   '�� �������� �������� �����.������� '
				  INTO account_name
				  FROM QQB_HL_SPR_SUBMANBA t, bank_desc_glb b
				 WHERE t.id = iSubmanbaId
					   AND b.code = iMfo;
				--
				oAccountCode := '11' || setup.get_local_code ||
								substr(v_acc_foiz_fil, 1, 9) ||
								bosh_unqcode || tartibNo;
				BEGIN
					SELECT a.code
					  INTO f_foiz_code
					  FROM accounts a
					 WHERE a.code LIKE oAccountCode;
				EXCEPTION
					WHEN no_data_found THEN
						SELECT c.client_uid
						  INTO vClUId
						  FROM client_current c
						 WHERE c.code = filial_unqcode
							   AND rownum = 1;
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
					
				END;
		END;
		--��� �� qqb_hl_accounts�� ����������
		BEGIN
			SELECT a.code, a.acc_external, a.id, a.code_coa
			  INTO f_foiz_code, v_acc_external, v_id, v_code_coa
			  FROM accounts a
			 WHERE a.code = f_foiz_code;
			IF SQL%FOUND
			THEN
				UPDATE dep_accounts
				   SET date_next = setup.get_operday
				 WHERE contract_id = f_id
					   AND account_type = 4
					   AND date_next = to_date('31.12.9999', 'dd.mm.yyyy');
				INSERT INTO dep_accounts
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
				VALUES
					(f_id,
					 4,
					 setup.get_operday,
					 iMfo,
					 f_foiz_code,
					 vCode_b,
					 SYSDATE,
					 to_date('31.12.9999', 'dd.mm.yyyy'),
					 v_id,
					 v_code_coa);
				INSERT INTO dep_accounts_his
					(contract_id,
					 account_type,
					 date_validate,
					 filial_code,
					 account_code,
					 emp_code,
					 date_modify,
					 action)
				VALUES
					(f_id,
					 4,
					 setup.get_operday,
					 iMfo,
					 f_foiz_code,
					 vCode_b,
					 SYSDATE,
					 'I');
				UPDATE qqb_hl_accounts h
				   SET h.acc_foiz_fil = v_acc_external
				 WHERE h.t2_id = iSubmanbaId
					   AND h.filial = iMfo;
				--shu erga 5,17101840,17101 larni ham biriktir
			END IF;
		EXCEPTION
			WHEN OTHERS THEN
				NULL;
		END;
	
		--Filialda 5- ��� ���� (54904000)
		BEGIN
			SELECT a.code, a.condition, a.id, a.code_coa
			  INTO f_acc_code, v_acc_condition, v_id, v_code_coa
			  FROM accounts a
			 WHERE a.code_filial = iMfo
				   AND a.code_coa = '54904'
				   AND a.code_currency = '000'
				   AND a.client_code = filial_unqcode
				   AND substr(a.acc_external, -3) = '103';
		EXCEPTION
			WHEN no_data_found THEN
				--�������� da 54904 ochish;
				setup.Set_Employee_Code(vCode_f);
				--tartib nomer
				tartibNo := '103';
				--schet nomi 
				account_name := '���� �� ��� �� ����� ������ ��������� ��������� ���������� ������������� ���� ����������� ������ ����������';
				--
				oAccountCode := '11' || setup.get_local_code ||
								'54904000_' || filial_unqcode || tartibNo;
				BEGIN
					SELECT a.code
					  INTO f_acc_code_5
					  FROM accounts a
					 WHERE a.code LIKE oAccountCode;
				EXCEPTION
					WHEN no_data_found THEN
						SELECT c.client_uid
						  INTO vClUId
						  FROM client_current c
						 WHERE c.code = filial_unqcode
							   AND rownum = 1;
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
				END;
		END;
		--��� �� qqb_hl_accounts�� ����������
		BEGIN
			SELECT a.code, a.acc_external, a.id, a.code_coa
			  INTO f_acc_code_5, v_acc_external, v_id, v_code_coa
			  FROM accounts a
			 WHERE a.code = f_acc_code_5;
			IF SQL%FOUND
			THEN
				UPDATE dep_accounts
				   SET date_next = setup.get_operday
				 WHERE contract_id = f_id
					   AND account_type = 5
					   AND date_next = to_date('31.12.9999', 'dd.mm.yyyy');
				INSERT INTO dep_accounts
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
				VALUES
					(f_id,
					 5,
					 setup.get_operday,
					 iMfo,
					 f_acc_code_5,
					 vCode_b,
					 SYSDATE,
					 to_date('31.12.9999', 'dd.mm.yyyy'),
					 v_id,
					 v_code_coa);
				INSERT INTO dep_accounts_his
					(contract_id,
					 account_type,
					 date_validate,
					 filial_code,
					 account_code,
					 emp_code,
					 date_modify,
					 action)
				VALUES
					(f_id,
					 5,
					 setup.get_operday,
					 iMfo,
					 f_acc_code_5,
					 vCode_b,
					 SYSDATE,
					 'I');
				UPDATE qqb_hl_accounts h
				   SET h.acc_res_fil = v_acc_external
				 WHERE h.t2_id = iSubmanbaId
					   AND h.filial = iMfo;
			END IF;
		EXCEPTION
			WHEN OTHERS THEN
				NULL;
		END;
		--Filialda 11- ��� ���� (17101000)
		BEGIN
			SELECT a.code, a.condition, a.id, a.code_coa
			  INTO f_acc_code, v_acc_condition, v_id, v_code_coa
			  FROM accounts a
			 WHERE a.code_filial = iMfo
				   AND a.code_coa = '17101'
				   AND a.code_currency = '000'
				   AND a.client_code = bosh_unqcode
				   AND
				   substr(a.acc_external, -3) = substr(v_acc_res_fil, -3);
		EXCEPTION
			WHEN no_data_found THEN
				--�������� da 17101 ochish;
				setup.Set_Employee_Code(vCode_f);
				--tartib nomer
				tartibNo := substr(v_acc_res_fil, -3);
				--schet nomi
				SELECT t.name ||
					   ' ������ ������� ������ ��� ������� ������� ���������'
				  INTO account_name
				  FROM QQB_HL_SPR_SUBMANBA t
				 WHERE t.id = iSubmanbaId;
				--
				oAccountCode := '11' || setup.get_local_code ||
								'17101000_' || bosh_unqcode || tartibNo;
				BEGIN
					SELECT a.code
					  INTO f_acc_code_11
					  FROM accounts a
					 WHERE a.code LIKE oAccountCode;
				EXCEPTION
					WHEN no_data_found THEN
						SELECT c.client_uid
						  INTO vClUId
						  FROM client_current c
						 WHERE c.code = filial_unqcode
							   AND rownum = 1;
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
				END;
		END;
		--��� �� qqb_hl_accounts�� ����������
		BEGIN
			SELECT a.code, a.acc_external, a.id, a.code_coa
			  INTO f_acc_code_11, v_acc_external, v_id, v_code_coa
			  FROM accounts a
			 WHERE a.code = f_acc_code_11;
			IF SQL%FOUND
			THEN
				UPDATE dep_accounts
				   SET date_next = setup.get_operday
				 WHERE contract_id = f_id
					   AND account_type = 11
					   AND date_next = to_date('31.12.9999', 'dd.mm.yyyy');
				INSERT INTO dep_accounts
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
				VALUES
					(f_id,
					 11,
					 setup.get_operday,
					 iMfo,
					 f_acc_code_11,
					 vCode_b,
					 SYSDATE,
					 to_date('31.12.9999', 'dd.mm.yyyy'),
					 v_id,
					 v_code_coa);
				INSERT INTO dep_accounts_his
					(contract_id,
					 account_type,
					 date_validate,
					 filial_code,
					 account_code,
					 emp_code,
					 date_modify,
					 action)
				VALUES
					(f_id,
					 11,
					 setup.get_operday,
					 iMfo,
					 f_acc_code_11,
					 vCode_b,
					 SYSDATE,
					 'I');
				UPDATE qqb_hl_accounts h
				   SET h.acc_res_fil = v_acc_external
				 WHERE h.t2_id = iSubmanbaId
					   AND h.filial = iMfo;
			END IF;
		EXCEPTION
			WHEN OTHERS THEN
				NULL;
		END;
		--Filialda 12- ��� ���� (17101840)
		BEGIN
			SELECT a.code, a.condition, a.id, a.code_coa
			  INTO f_acc_code, v_acc_condition, v_id, v_code_coa
			  FROM accounts a
			 WHERE a.code_filial = iMfo
				   AND a.code_coa = '17101'
				   AND a.code_currency = '840'
				   AND a.client_code = bosh_unqcode
				   AND
				   substr(a.acc_external, -3) = substr(v_acc_res_fil, -3);
		EXCEPTION
			WHEN no_data_found THEN
				--�������� da 17101 ochish;
				setup.Set_Employee_Code(vCode_f);
				--tartib nomer
				tartibNo := substr(v_acc_res_fil, -3);
				--schet nomi
				SELECT t.name ||
					   ' ������ ������� ������ ��� ������� ������� ���������'
				  INTO account_name
				  FROM QQB_HL_SPR_SUBMANBA t
				 WHERE t.id = iSubmanbaId;
				--
				oAccountCode := '11' || setup.get_local_code ||
								'17101840_' || bosh_unqcode || tartibNo;
				BEGIN
					SELECT a.code
					  INTO f_acc_code_12
					  FROM accounts a
					 WHERE a.code LIKE oAccountCode;
				EXCEPTION
					WHEN no_data_found THEN
						SELECT c.client_uid
						  INTO vClUId
						  FROM client_current c
						 WHERE c.code = filial_unqcode
							   AND rownum = 1;
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
				END;
		END;
		--��� �� qqb_hl_accounts�� ����������
		BEGIN
			SELECT a.code, a.acc_external, a.id, a.code_coa
			  INTO f_acc_code_12, v_acc_external, v_id, v_code_coa
			  FROM accounts a
			 WHERE a.code = f_acc_code_12;
			IF SQL%FOUND
			THEN
				UPDATE dep_accounts
				   SET date_next = setup.get_operday
				 WHERE contract_id = f_id
					   AND account_type = 12
					   AND date_next = to_date('31.12.9999', 'dd.mm.yyyy');
				INSERT INTO dep_accounts
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
				VALUES
					(f_id,
					 12,
					 setup.get_operday,
					 iMfo,
					 f_acc_code_12,
					 vCode_b,
					 SYSDATE,
					 to_date('31.12.9999', 'dd.mm.yyyy'),
					 v_id,
					 v_code_coa);
				INSERT INTO dep_accounts_his
					(contract_id,
					 account_type,
					 date_validate,
					 filial_code,
					 account_code,
					 emp_code,
					 date_modify,
					 action)
				VALUES
					(f_id,
					 12,
					 setup.get_operday,
					 iMfo,
					 f_acc_code_12,
					 vCode_b,
					 SYSDATE,
					 'I');
				UPDATE qqb_hl_accounts h
				   SET h.acc_res_fil = v_acc_external
				 WHERE h.t2_id = iSubmanbaId
					   AND h.filial = iMfo;
			END IF;
		EXCEPTION
			WHEN OTHERS THEN
				NULL;
		END;
	END;
	--Nazarov X.A 28.09.2020
	PROCEDURE provodka
	(
		s_id     glb_cred_hl_summa.id%TYPE
	   ,oMessage OUT VARCHAR2
	) IS
		b_id             dep_contracts.id%TYPE;
		f_id             dep_contracts.id%TYPE;
		vCode_o          core_users.user_id%TYPE;
		vCode_b          core_users.user_id%TYPE;
		iis_preview_mode VARCHAR2(3) := 'OFF';
		iContracts       array_varchar2;
		iParams_code     array_varchar2;
		iParams_value    array_varchar2;
		b_mfo            VARCHAR2(5);
		b_sch            VARCHAR2(27);
		b_sch16102       VARCHAR2(20);
		f_mfo            VARCHAR2(5);
		f_sch            VARCHAR2(27);
		f_sch22203       VARCHAR2(20);
		v_state          glb_cred_hl_summa.state%TYPE;
		v_ruxsat         glb_cred_hl_summa.ruhsat_sum%TYPE;
		v_nazn           leads_cur.pay_purpose%TYPE;
		v_id_resurs      qqb_hl_resurs.id%TYPE;
		vid_zayav        glb_cred_hl_summa.id_zayav%TYPE;
		v_code_val       dep_contracts.currency_code%TYPE;
		------------------------------------------------------
		vOperDay DATE := setup.get_operday;
		vFarNumb VARCHAR2(10);
	BEGIN
		--------------------------Fozil---------------------------------
		BEGIN
			SELECT f.far_numb
			  INTO vFarNumb
			  FROM qqb_hl_spr_farmoyish f
			 WHERE f.far_date = vOperDay;
		EXCEPTION
			WHEN no_data_found THEN
				Raise_Application_Error(-20000,
										'����� ' || vOperDay ||
										' ���� ���� �������� ��������!(��������� -> �����������)');
		END;
		--------------------------Fozil---------------------------------
		BEGIN
			SELECT m.state, nvl(m.ruhsat_sum, 0), id_resurs, m.id_zayav
			  INTO v_state, v_ruxsat, v_id_resurs, vid_zayav
			  FROM glb_cred_hl_summa m
			 WHERE m.id = s_id;
			IF SQL%FOUND
			THEN
				IF v_state <> 1
				THEN
					Raise_Application_Error(-20000,
											'������ ����������!');
				END IF;
			END IF;
		EXCEPTION
			WHEN OTHERS THEN
				Raise_Application_Error(-20000, SQLERRM);
		END;
		BEGIN
			SELECT r.dep_id_b, r.dep_id_f
			  INTO b_id, f_id
			  FROM qqb_hl_resurs r
			 WHERE r.id = v_id_resurs
				   AND r.zv_id = vid_zayav;
		EXCEPTION
			WHEN OTHERS THEN
				Raise_Application_Error(-20000, SQLERRM);
		END;
		--currency
		BEGIN
			SELECT d.currency_code
			  INTO v_code_val
			  FROM dep_contracts d
			 WHERE d.id = b_id;
		EXCEPTION
			WHEN no_data_found THEN
				oMessage := '�������� ���������';
				RETURN;
			WHEN too_many_rows THEN
				oMessage := '�������� ��� �����';
				RETURN;
			WHEN OTHERS THEN
				oMessage := '���������� ��������� �������';
				RETURN;
		END;
		--naznacheniya
		BEGIN
			SELECT (SELECT d.contract_name || '�� '
					  FROM dep_contracts d
					 WHERE d.id = r.dep_id_b) ||
				   (SELECT b.name || ' ������� ������ ������� ������ '
					  FROM qqb_hl_spr_submanba b
					 WHERE b.ID = r.submanba_id) || '� ' || vFarNumb ||
				   ' ���������� ������ ' ||
				   (SELECT d.count_days || ' ����� '
					  FROM dep_contracts d
					 WHERE d.id = r.dep_id_b) ||
				   (SELECT '������ ' || to_char(p.percent_rate) ||
						   ' ���� �������� ���������� �����'
					  FROM dep_contracts_percent_rate p
					 WHERE p.contract_id = r.dep_id_b
						   AND p.percent_type = 'DEP'
						   AND
						   p.date_validate =
						   (SELECT MAX(p1.date_validate)
							  FROM dep_contracts_percent_rate p1
							 WHERE p1.contract_id = p.contract_id
								   AND p1.percent_type = p.percent_type)) procet
			  INTO v_nazn
			  FROM qqb_hl_resurs r
			 WHERE r.id = v_id_resurs;
		EXCEPTION
			WHEN no_data_found THEN
				oMessage := '����� ������� ���������';
				RETURN;
			WHEN too_many_rows THEN
				oMessage := '����� ������� ��� �����';
				RETURN;
			WHEN OTHERS THEN
				oMessage := '����� �������  ����';
				RETURN;
		END;
		--��� ���� ������� (16102) �������
		BEGIN
			SELECT d.filial_code,
				   d.account_code,
				   substr(d.account_code, -20)
			  INTO b_mfo, b_sch, b_sch16102
			  FROM dep_accounts d
			 WHERE d.contract_id = b_id
				   AND d.account_type = 1
				   AND d.date_next = to_date('31.12.9999', 'dd.mm.yyyy');
		EXCEPTION
			WHEN no_data_found THEN
				oMessage := 'bosh bank sceti topilmadi';
				RETURN;
			WHEN too_many_rows THEN
				oMessage := 'bosh bank sceti bir nechta';
				RETURN;
			WHEN OTHERS THEN
				oMessage := 'bosh bank sceti xato';
				RETURN;
		END;
		--������  ������� (22203) �������
		BEGIN
			SELECT d.filial_code,
				   d.account_code,
				   substr(d.account_code, -20)
			  INTO f_mfo, f_sch, f_sch22203
			  FROM dep_accounts d
			 WHERE d.contract_id = f_id
				   AND d.account_type = 1
				   AND d.date_next = to_date('31.12.9999', 'dd.mm.yyyy');
		EXCEPTION
			WHEN no_data_found THEN
				oMessage := 'filail sceti topilmadi';
				RETURN;
			WHEN too_many_rows THEN
				oMessage := 'filail sceti bir nechta';
				RETURN;
			WHEN OTHERS THEN
				oMessage := 'filail sceti xato';
				RETURN;
		END;
		--��� ���� ��� ��������� ���� �� ���� ������(�������� ����� ���� �����)
		SELECT setup.get_employee_code INTO vCode_o FROM dual;
		SELECT p.code
		  INTO vCode_b
		  FROM v_emp_pass p
		 WHERE p.filial_code = b_mfo
			   AND p.rank_code = 4
			   AND p.condition = 'A';
		setup.set_employee_code(vCode_b);
		--v_ruxsat ������� ������ ������� ,���� �������� ����� (��: 16102 �-� 22203)
		iContracts    := array_varchar2();
		iParams_code  := array_varchar2();
		iParams_value := array_varchar2();
		iContracts    := sv_budjet_zp.add_in_array(iContracts, b_id);
		iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
												   'LEAD_ACCOUNT');
		iParams_value := sv_budjet_zp.add_in_array(iParams_value,
												   f_sch22203);
		iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
												   'LEAD_ACCOUNT_DEBET');
		iParams_value := sv_budjet_zp.add_in_array(iParams_value,
												   b_sch16102);
		iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
												   'LEAD_ACCOUNT_NAME');
		iParams_value := sv_budjet_zp.add_in_array(iParams_value,
												   bank.get_account_name(f_sch));
		iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
												   'LEAD_ACCOUNT_NAME_DEBET');
		iParams_value := sv_budjet_zp.add_in_array(iParams_value,
												   bank.get_account_name(b_sch));
		iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
												   'LEAD_ACT_ID');
		iParams_value := sv_budjet_zp.add_in_array(iParams_value, '11');
		iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
												   'LEAD_DOC_NUMB');
		iParams_value := sv_budjet_zp.add_in_array(iParams_value, '1');
		iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
												   'LEAD_FILIAL');
		iParams_value := sv_budjet_zp.add_in_array(iParams_value, b_mfo);
		iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
												   'LEAD_FILIAL_CREDIT');
		iParams_value := sv_budjet_zp.add_in_array(iParams_value, f_mfo);
		iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
												   'LEAD_FILIAL_NAME');
		iParams_value := sv_budjet_zp.add_in_array(iParams_value,
												   '������� �., ��� "������ ������� ����" ��� ����Ȩ���� ����������');
		iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
												   'LEAD_INN');
		iParams_value := sv_budjet_zp.add_in_array(iParams_value,
												   bank.get_inn_acc(f_sch));
		iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
												   'LEAD_INN_DEBET');
		iParams_value := sv_budjet_zp.add_in_array(iParams_value,
												   bank.get_inn_acc(b_sch));
		iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
												   'LEAD_PURPOSE');
		iParams_value := sv_budjet_zp.add_in_array(iParams_value, v_nazn);
		iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
												   'LEAD_SUM_PAY');
		iParams_value := sv_budjet_zp.add_in_array(iParams_value,
												   TRIM(to_char(v_ruxsat,
																'999999999999.99')));
		iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
												   'LEAD_SUM_PAY_IN_WORDS');
		iParams_value := sv_budjet_zp.add_in_array(iParams_value,
												   vkl_rep.summa_suz(100 *
																	 v_ruxsat,
																	 v_code_val));
		iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
												   'LEAD_SYM_ID');
		iParams_value := sv_budjet_zp.add_in_array(iParams_value, '56061');
		iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
												   'LEAD_TRANS_ID');
		iParams_value := sv_budjet_zp.add_in_array(iParams_value, '106');
		iParams_code  := sv_budjet_zp.add_in_array(iParams_code,
												   'LOAN_CURRENCY');
		iParams_value := sv_budjet_zp.add_in_array(iParams_value,
												   v_code_val);
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
				FOR i IN 1 .. iContracts.Count
				LOOP
					INSERT INTO vkl_proc_error
						(mfo, id_account, error_message, nachis_day)
					VALUES
						(setup.Get_Filial_Code,
						 iContracts(i),
						 'HAND_dd' || oMessage,
						 setup.Get_Operday);
				END LOOP;
		END;
		setup.set_employee_code(vCode_o);
	END;
	PROCEDURE accept_test IS
	
		v_dep_id_b          INTEGER;
		v_dep_id_f          INTEGER;
		v_error             VARCHAR2(32767);
		v_action_resource   dep_s_action_resource.code%TYPE;
		v_account_layouts   dep_account_layouts.id%TYPE; --������ ������; ������. ������� 16102-840
		v_deposit_type      dep_s_deposit_type.code%TYPE; --���������� ����� �������; ������������� �������
		v_contract_date     dep_contracts.contract_date%TYPE;
		v_date_end          dep_contracts.date_end%TYPE;
		v_currency_code     dep_contracts.currency_code%TYPE;
		v_desc_days_in_year DEP_s_DAYS_IN_YEAR.Code%TYPE; --���������� ���������� ����;
	
		v_summa      dep_contracts.summa%TYPE;
		v_cl_code    dep_contracts.client_code%TYPE;
		v_loan_id    ln_card.loan_id%TYPE;
		v_count_days dep_contracts.count_days%TYPE;
		v_date_begin ln_card.open_date%TYPE;
	
		vEmpCode                core_users.user_id%TYPE; --current
		vEmpHeaderCode          core_users.user_id%TYPE; --bosh bank
		vEmpZvCode              core_users.user_id%TYPE; --zayavka
		v_mfo                   CHAR(5);
		i_submanba_id           NUMBER;
		i_contract_name         dep_contracts.contract_name%TYPE;
		i_contract_number       dep_contracts.contract_number%TYPE;
		i_percent               DEP_CONTRACTS_PERCENT_RATE.PERCENT_RATE%TYPE;
		i_percent_DATE_VALIDATE VARCHAR2(255);
		i_Libor_check           VARCHAR2(2);
		i_resurs_id             qqb_hl_resurs.id%TYPE;
		o_Message               VARCHAR2(255);
		tartibNo                CHAR(10);
	
	BEGIN
		BEGIN
			SELECT a.user_id
			  INTO vEmpHeaderCode
			  FROM core_users a
			 WHERE a.filial_code = '01037'
				   AND a.private_post_id = 4
				   AND a.state = 'A';
			v_date_begin := to_date(to_char(SYSDATE - 1, 'dd.mm.yyyy'),
									'dd.mm.yyyy');
			v_date_end   := to_date(to_char(SYSDATE + 364, 'dd.mm.yyyy'),
									'dd.mm.yyyy');
			SELECT v_date_end - v_date_begin INTO v_count_days FROM dual;
			v_contract_date         := v_date_begin;
			v_currency_code         := '840';
			v_desc_days_in_year     := 2;
			i_Libor_check           := 'N';
			i_percent_DATE_VALIDATE := '12.11.2020';
			FOR e IN (SELECT *
						FROM qqb_hl_resurs t
					   WHERE t.zv_id IN (-73)
					   ORDER BY t.zv_id DESC)
			LOOP
				dbms_output.put_line('RESURS ID=' || e.id);
				v_mfo         := e.filial_code;
				v_summa       := e.summa;
				i_submanba_id := e.submanba_id;
				SELECT 'MF' || lpad(to_char(abs(e.zv_id)), 3, '0')
				  INTO i_contract_number
				  FROM dual;
				SELECT to_number(REPLACE(e.izox, ',', '.'))
				  INTO i_percent
				  FROM dual;
			
				i_contract_name := v_mfo || '---' || i_contract_number;
				i_resurs_id     := e.id;
				tartibNo        := e.resurs_number;
				IF i_submanba_id IN (24, 27, 28, 29, 30)
				THEN
					v_currency_code := '978';
				END IF;
				SELECT a.user_id
				  INTO vEmpZvCode
				  FROM core_users a
				 WHERE a.filial_code = v_mfo
					   AND a.private_post_id = 4
					   AND a.state = 'A';
			
				----Shartnoma; ����������
				setup.set_employee_code(vEmpHeaderCode);
				v_action_resource := 'PLACEMENT';
				IF v_currency_code = '978'
				THEN
					v_account_layouts := 74; --������ ������; ������. ������� 16102-978
				END IF;
				IF v_currency_code = '840'
				THEN
					v_account_layouts := 63; --������ ������; ������. ������� 16102-840
				END IF;
				v_deposit_type := 4; --���������� ����� �������; ������������� �������
				contract_action(i_action_resource => v_action_resource,
								
								i_client_code           => lpad(v_mfo,
																8,
																'0'),
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
				IF v_error IS NOT NULL
				THEN
					ROLLBACK;
					o_Message := v_error;
					Raise_Application_Error(-20000, v_error);
				ELSE
					----Shartnoma; �����������
					setup.set_employee_code(vEmpZvCode);
					IF v_currency_code = '978'
					THEN
						v_account_layouts := 76; --������ ������; ������. ������� 16102-978
					END IF;
					IF v_currency_code = '840'
					THEN
						v_account_layouts := 62; --������ ������; ������. ������� 16102-840
					END IF;
					v_action_resource := 'ATTRACT';
					v_deposit_type    := 4; --���������� ����� �������; ������������� �������
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
				
					IF v_error IS NOT NULL
					THEN
						o_Message := v_error;
						ROLLBACK;
						Raise_Application_Error(-20000, v_error);
					ELSE
						---Shaxzod qo'shgan grafik genratsiya qiladi
						/*generator_graph(iLoan_id                => v_Loan_id,
                        iDep_contract_id        => v_dep_id_b,
                        IDep_filial_contract_id => v_dep_id_f);*/
					
						---��� �� ��� ����������
						UPDATE qqb_hl_resurs t
						   SET t.submanba_id = i_submanba_id,
							   t.dep_id_b    = v_dep_id_b,
							   t.dep_id_f    = v_dep_id_f,
							   t.state       = 2
						 WHERE t.id = i_resurs_id;
						--Nazarov X.A 28.09.2020
						--- ��������� ����� �����
						--�������� ����
						INSERT INTO dep_contracts_mode_actions
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
						VALUES
							(v_dep_id_b,
							 'ACD',
							 'MANUAL',
							 'N',
							 'N',
							 vEmpHeaderCode,
							 SYSDATE,
							 'Y',
							 'USUAL',
							 'N');
						INSERT INTO dep_contracts_mode_actions_his
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
						VALUES
							(v_dep_id_b,
							 'ACD',
							 'MANUAL',
							 'N',
							 'N',
							 vEmpHeaderCode,
							 SYSDATE,
							 'Y',
							 'USUAL',
							 'N');
						--������
						INSERT INTO dep_contracts_mode_actions
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
						VALUES
							(v_dep_id_f,
							 'ACD',
							 'MANUAL',
							 'N',
							 'N',
							 vEmpZvCode,
							 SYSDATE,
							 'Y',
							 'USUAL',
							 'N');
						INSERT INTO dep_contracts_mode_actions_his
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
						VALUES
							(v_dep_id_f,
							 'ACD',
							 'MANUAL',
							 'N',
							 'N',
							 vEmpZvCode,
							 SYSDATE,
							 'Y',
							 'USUAL',
							 'N');
						--������� ����������. ��� ����� ���� ����������
						openBindAcc(v_mfo,
									i_submanba_id,
									v_dep_id_b,
									v_dep_id_f,
									o_Message);
						-- Sherzodaka yozgan protokolni chaqirilgan
						qqb_resurs_val.prot(i_resurs_id,
											'������ �������',
											2,
											3,
											SYSDATE);
						--o_Message := 'Ma`lumotlar muvaffaqiyatli saqlandi';
					END IF;
				END IF;
			END LOOP;
		EXCEPTION
			WHEN OTHERS THEN
				o_Message := SQLERRM;
				Raise_Application_Error(-20000, SQLERRM);
		END;
	END accept_test;
	PROCEDURE openBindAcc_TEST
	(
		iMfo        VARCHAR2
	   ,iSubmanbaId INTEGER
	   ,b_id        dep_contracts.id%TYPE
	   ,f_id        dep_contracts.id%TYPE
	   ,oMessage    OUT VARCHAR2
	   ,tartibNo    VARCHAR2
	) IS
		v_acc_res_bo    accounts.acc_external%TYPE;
		v_acc_foiz_bo   accounts.acc_external%TYPE;
		v_acc_res_fil   accounts.acc_external%TYPE;
		v_acc_foiz_fil  accounts.acc_external%TYPE;
		v_acc_code      accounts.code%TYPE;
		f_acc_code      accounts.code%TYPE;
		v_acc_external  accounts.acc_external%TYPE;
		v_acc_condition accounts.condition%TYPE;
		vCode_b         v_emp_pass.code%TYPE;
		vCode_f         v_emp_pass.code%TYPE;
		iCondition      client_current.condition%TYPE;
		vClUId          client_current.client_uid%TYPE;
		filial_unqcode  accounts.client_code%TYPE;
		bosh_unqcode    accounts.client_code%TYPE := '00001037';
		account_name    accounts.name%TYPE;
		oAccountCode    ACCOUNTS.CODE%TYPE;
		iOperation      S_ACC_OPERATION.OPERATION%TYPE;
		oCondition      S_ACC_OPERATION.CONDITION_RESULT%TYPE;
		oDialog         S_ACC_OPERATION.DIALOG_NEED%TYPE;
		boshMfo         VARCHAR2(5) := '01037';
		v_id            accounts.id%TYPE;
		v_code_coa      accounts.code_coa%TYPE;
		v_foiz_code     accounts.code%TYPE;
		f_foiz_code     accounts.code%TYPE;
	BEGIN
		SELECT p.code
		  INTO vCode_b
		  FROM v_emp_pass p
		 WHERE p.filial_code = boshMfo
			   AND p.rank_code = 4
			   AND p.condition = 'A';
		SELECT p.code
		  INTO vCode_f
		  FROM v_emp_pass p
		 WHERE p.filial_code = iMfo
			   AND p.rank_code = 4
			   AND p.condition = 'A';
		filial_unqcode := lpad(iMfo, 8, '0');
		BEGIN
			SELECT t.acc_res_bo,
				   t.acc_foiz_bo,
				   t.acc_res_fil,
				   t.acc_foiz_fil
			  INTO v_acc_res_bo,
				   v_acc_foiz_bo,
				   v_acc_res_fil,
				   v_acc_foiz_fil
			  FROM QQB_HL_ACCOUNTS t
			 WHERE t.filial = iMfo
				   AND t.t2_id = iSubmanbaId;
			IF SQL%FOUND
			THEN
				SELECT c.condition
				  INTO iCondition
				  FROM client_current c
				 WHERE c.code = filial_unqcode;
				IF iCondition = 'T'
				THEN
					UPDATE client_current t
					   SET t.condition             = 'A',
						   t.date_validate         = SYSDATE,
						   t.date_change_condition = setup.get_operday
					 WHERE t.code = filial_unqcode;
					INSERT INTO client_history
						SELECT *
						  FROM client_current
						 WHERE code = filial_unqcode;
				END IF;
				SELECT c.condition
				  INTO iCondition
				  FROM client_current c
				 WHERE c.code = bosh_unqcode;
				IF iCondition = 'T'
				THEN
					UPDATE client_current t
					   SET t.condition             = 'A',
						   t.date_validate         = SYSDATE,
						   t.date_change_condition = setup.get_operday
					 WHERE t.code = bosh_unqcode;
					INSERT INTO client_history
						SELECT *
						  FROM client_current
						 WHERE code = bosh_unqcode;
				END IF;
			
			END IF;
		EXCEPTION
			WHEN no_data_found THEN
				oMessage := 'QQB_HL_ACCOUNTS ��������� ������ ���������?!';
				RETURN;
		END;
		--Bosh bankda asosiy schet(16102)
		BEGIN
			SELECT a.code, a.condition, a.id, a.code_coa
			  INTO v_acc_code, v_acc_condition, v_id, v_code_coa
			  FROM accounts a
			 WHERE a.code_filial = boshMfo
				   AND a.acc_external = v_acc_res_bo;
		EXCEPTION
			WHEN no_data_found THEN
				--01037 da 16102 ochish;
				setup.Set_Employee_Code(vCode_b);
				--schet nomi
				SELECT substr(t.name, 1, 30) || ' ������ ������� ������ ' ||
					   substr(b.name, 1, 30) || '�� �������� ���������'
				  INTO account_name
				  FROM QQB_HL_SPR_SUBMANBA t, bank_desc_glb b
				 WHERE t.id = iSubmanbaId
					   AND b.code = iMfo;
				--
				oAccountCode := '11' || setup.get_local_code ||
								substr(v_acc_res_bo, 1, 9) ||
								filial_unqcode || tartibNo;
				BEGIN
					SELECT a.code
					  INTO v_acc_code
					  FROM accounts a
					 WHERE a.code LIKE oAccountCode;
				EXCEPTION
					WHEN no_data_found THEN
						SELECT substr(oAccountCode, 17, 8)
						  INTO vClUId
						  FROM dual;
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
					
				END;
		END;
		--��� �� qqb_hl_accounts�� ����������
		BEGIN
			SELECT a.code, a.acc_external, a.id, a.code_coa
			  INTO v_acc_code, v_acc_external, v_id, v_code_coa
			  FROM accounts a
			 WHERE a.code = v_acc_code;
			IF SQL%FOUND
			THEN
				UPDATE dep_accounts
				   SET date_next = setup.get_operday
				 WHERE contract_id = b_id
					   AND account_type = 1
					   AND date_next = to_date('31.12.9999', 'dd.mm.yyyy');
				INSERT INTO dep_accounts
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
				VALUES
					(b_id,
					 1,
					 setup.get_operday,
					 boshMfo,
					 v_acc_code,
					 vCode_b,
					 SYSDATE,
					 to_date('31.12.9999', 'dd.mm.yyyy'),
					 v_id,
					 v_code_coa);
				INSERT INTO dep_accounts_his
					(contract_id,
					 account_type,
					 date_validate,
					 filial_code,
					 account_code,
					 emp_code,
					 date_modify,
					 action)
				VALUES
					(b_id,
					 1,
					 setup.get_operday,
					 boshMfo,
					 v_acc_code,
					 vCode_b,
					 SYSDATE,
					 'I');
				UPDATE qqb_hl_accounts h
				   SET h.acc_res_bo = v_acc_external
				 WHERE h.t2_id = iSubmanbaId
					   AND h.filial = iMfo;
			END IF;
		EXCEPTION
			WHEN OTHERS THEN
				NULL;
		END;
		--Bosh bankda procent schet(16304)
		BEGIN
			SELECT a.code, a.condition, a.id, a.code_coa
			  INTO v_foiz_code, v_acc_condition, v_id, v_code_coa
			  FROM accounts a
			 WHERE a.code_filial = boshMfo
				   AND a.acc_external = v_acc_foiz_bo;
		EXCEPTION
			WHEN no_data_found THEN
				--01037 da 16304 ochish;
				setup.Set_Employee_Code(vCode_b);
				--schet nomi
				SELECT substr(t.name, 1, 30) || ' ������ ������� ������ ' ||
					   substr(b.name, 1, 30) ||
					   '�� �������� �������� �����.������� '
				  INTO account_name
				  FROM QQB_HL_SPR_SUBMANBA t, bank_desc_glb b
				 WHERE t.id = iSubmanbaId
					   AND b.code = iMfo;
				--
				oAccountCode := '11' || setup.get_local_code ||
								substr(v_acc_foiz_bo, 1, 9) ||
								filial_unqcode || tartibNo;
				BEGIN
					SELECT a.code
					  INTO v_foiz_code
					  FROM accounts a
					 WHERE a.code LIKE oAccountCode;
				EXCEPTION
					WHEN no_data_found THEN
						SELECT substr(oAccountCode, 17, 8)
						  INTO vClUId
						  FROM dual;
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
					
				END;
		END;
		--��� �� qqb_hl_accounts�� ����������
		BEGIN
			SELECT a.code, a.acc_external, a.id, a.code_coa
			  INTO v_foiz_code, v_acc_external, v_id, v_code_coa
			  FROM accounts a
			 WHERE a.code = v_foiz_code;
			IF SQL%FOUND
			THEN
				UPDATE dep_accounts
				   SET date_next = setup.get_operday
				 WHERE contract_id = b_id
					   AND account_type = 4
					   AND date_next = to_date('31.12.9999', 'dd.mm.yyyy');
				INSERT INTO dep_accounts
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
				VALUES
					(b_id,
					 4,
					 setup.get_operday,
					 boshMfo,
					 v_foiz_code,
					 vCode_b,
					 SYSDATE,
					 to_date('31.12.9999', 'dd.mm.yyyy'),
					 v_id,
					 v_code_coa);
				INSERT INTO dep_accounts_his
					(contract_id,
					 account_type,
					 date_validate,
					 filial_code,
					 account_code,
					 emp_code,
					 date_modify,
					 action)
				VALUES
					(b_id,
					 4,
					 setup.get_operday,
					 boshMfo,
					 v_foiz_code,
					 vCode_b,
					 SYSDATE,
					 'I');
				UPDATE qqb_hl_accounts h
				   SET h.acc_foiz_bo = v_acc_external
				 WHERE h.t2_id = iSubmanbaId
					   AND h.filial = iMfo;
			END IF;
			--shu erga 44905,17101840,17101 larni ham biriktir
		EXCEPTION
			WHEN OTHERS THEN
				NULL;
		END;
		--Filialda asosiy schet(22203)
		BEGIN
			SELECT a.code, a.condition, a.id, a.code_coa
			  INTO f_acc_code, v_acc_condition, v_id, v_code_coa
			  FROM accounts a
			 WHERE a.code_filial = iMfo
				   AND a.acc_external = v_acc_res_fil;
		EXCEPTION
			WHEN no_data_found THEN
				--�������� da 22203 ochish;
				setup.Set_Employee_Code(vCode_f);
				--schet nomi
				SELECT substr(t.name, 1, 40) ||
					   ' ������ ������� ������ ��� ������� �������  ���������'
				  INTO account_name
				  FROM QQB_HL_SPR_SUBMANBA t
				 WHERE t.id = iSubmanbaId;
				--
				oAccountCode := '11' || setup.get_local_code ||
								substr(v_acc_res_fil, 1, 9) ||
								bosh_unqcode || tartibNo;
				BEGIN
					SELECT a.code
					  INTO f_acc_code
					  FROM accounts a
					 WHERE a.code LIKE oAccountCode;
				EXCEPTION
					WHEN no_data_found THEN
						SELECT substr(oAccountCode, 17, 8)
						  INTO vClUId
						  FROM dual;
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
					
				END;
		END;
		--��� �� qqb_hl_accounts�� ����������
		BEGIN
			SELECT a.code, a.acc_external, a.id, a.code_coa
			  INTO f_acc_code, v_acc_external, v_id, v_code_coa
			  FROM accounts a
			 WHERE a.code = f_acc_code;
			IF SQL%FOUND
			THEN
				UPDATE dep_accounts
				   SET date_next = setup.get_operday
				 WHERE contract_id = f_id
					   AND account_type = 1
					   AND date_next = to_date('31.12.9999', 'dd.mm.yyyy');
				INSERT INTO dep_accounts
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
				VALUES
					(f_id,
					 1,
					 setup.get_operday,
					 iMfo,
					 f_acc_code,
					 vCode_b,
					 SYSDATE,
					 to_date('31.12.9999', 'dd.mm.yyyy'),
					 v_id,
					 v_code_coa);
				INSERT INTO dep_accounts_his
					(contract_id,
					 account_type,
					 date_validate,
					 filial_code,
					 account_code,
					 emp_code,
					 date_modify,
					 action)
				VALUES
					(f_id,
					 1,
					 setup.get_operday,
					 iMfo,
					 f_acc_code,
					 vCode_b,
					 SYSDATE,
					 'I');
				UPDATE qqb_hl_accounts h
				   SET h.acc_res_fil = v_acc_external
				 WHERE h.t2_id = iSubmanbaId
					   AND h.filial = iMfo;
			END IF;
		EXCEPTION
			WHEN OTHERS THEN
				NULL;
		END;
		--Filialda procent schet(22409)
		BEGIN
			SELECT a.code, a.condition, a.id, a.code_coa
			  INTO f_foiz_code, v_acc_condition, v_id, v_code_coa
			  FROM accounts a
			 WHERE a.code_filial = iMfo
				   AND a.acc_external = v_acc_foiz_fil;
		EXCEPTION
			WHEN no_data_found THEN
				--��������  22409 ochish;
				setup.Set_Employee_Code(vCode_f);
				--schet nomi
				SELECT substr(t.name, 1, 30) || ' ������ ������� ������ ' ||
					   substr(b.name, 1, 30) ||
					   '�� �������� �������� �����.������� '
				  INTO account_name
				  FROM QQB_HL_SPR_SUBMANBA t, bank_desc_glb b
				 WHERE t.id = iSubmanbaId
					   AND b.code = iMfo;
				--
				oAccountCode := '11' || setup.get_local_code ||
								substr(v_acc_foiz_fil, 1, 9) ||
								bosh_unqcode || tartibNo;
				BEGIN
					SELECT a.code
					  INTO f_foiz_code
					  FROM accounts a
					 WHERE a.code LIKE oAccountCode;
				EXCEPTION
					WHEN no_data_found THEN
						SELECT substr(oAccountCode, 17, 8)
						  INTO vClUId
						  FROM dual;
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
					
				END;
		END;
		--��� �� qqb_hl_accounts�� ����������
		BEGIN
			SELECT a.code, a.acc_external, a.id, a.code_coa
			  INTO f_foiz_code, v_acc_external, v_id, v_code_coa
			  FROM accounts a
			 WHERE a.code = f_foiz_code;
			IF SQL%FOUND
			THEN
				UPDATE dep_accounts
				   SET date_next = setup.get_operday
				 WHERE contract_id = f_id
					   AND account_type = 4
					   AND date_next = to_date('31.12.9999', 'dd.mm.yyyy');
				INSERT INTO dep_accounts
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
				VALUES
					(f_id,
					 4,
					 setup.get_operday,
					 iMfo,
					 f_foiz_code,
					 vCode_b,
					 SYSDATE,
					 to_date('31.12.9999', 'dd.mm.yyyy'),
					 v_id,
					 v_code_coa);
				INSERT INTO dep_accounts_his
					(contract_id,
					 account_type,
					 date_validate,
					 filial_code,
					 account_code,
					 emp_code,
					 date_modify,
					 action)
				VALUES
					(f_id,
					 4,
					 setup.get_operday,
					 iMfo,
					 f_foiz_code,
					 vCode_b,
					 SYSDATE,
					 'I');
				UPDATE qqb_hl_accounts h
				   SET h.acc_foiz_fil = v_acc_external
				 WHERE h.t2_id = iSubmanbaId
					   AND h.filial = iMfo;
				--shu erga 5,17101840,17101 larni ham biriktir
			END IF;
		EXCEPTION
			WHEN OTHERS THEN
				NULL;
		END;
	
	END;
	----------------------Fozil-----------------------------------------
	PROCEDURE far_add
	(
		iSon     qqb_hl_spr_farmoyish.far_numb%TYPE
	   ,iSana    qqb_hl_spr_farmoyish.far_date%TYPE
	   ,oMessage OUT VARCHAR2
	) IS
		vID qqb_hl_spr_farmoyish.id%TYPE := qqb_hl_spr_farmoyish_seq.nextval;
	BEGIN
		SELECT COUNT(*)
		  INTO vCount
		  FROM qqb_hl_spr_farmoyish g
		 WHERE g.far_numb = iSon
			   AND g.far_date = iSana;
		IF vCount = 0
		THEN
			INSERT INTO qqb_hl_spr_farmoyish
				(id, far_date, far_numb, mod_date)
			VALUES
				(vID, iSana, iSon, SYSDATE);
			oMessage := 'OK';
		ELSE
			oMessage := 'No';
		END IF;
	END;
	----------------------Fozil-----------------------------------------

	PROCEDURE vnebal_lead
	(
		iManba      NUMBER
	   ,iSubManba   NUMBER
	   ,iDtAcc      VARCHAR2
	   ,iKrAcc      VARCHAR2
	   ,iSumma      NUMBER
	   ,iPayPurpose VARCHAR2
	) IS
		Leadid   NUMBER;
		s20206   accounts%ROWTYPE;
		s29801   accounts%ROWTYPE;
		vCode    core_users.user_id%TYPE := 12376;
		vCodeOld core_users.user_id%TYPE;
		kk       VARCHAR2(2);
		vOperDay DATE := setup.Get_Operday;
		Err_Code NUMBER;
		Err_Msg  VARCHAR2(2000);
		nazn     VARCHAR2(2000);
		vMfo     VARCHAR2(5) := '01037';
		vCond    VARCHAR2(1);
	BEGIN
		BEGIN
			SELECT setup.Get_Employee_Code INTO vCodeOld FROM dual;
		
			setup.Set_Employee_Code(vCode);
		
			vkl_api.NEW_LEAD(doc_numb    => Leadid,
							 doc_date    => vOperDay,
							 cl_mfo      => vMfo,
							 cl_acc      => iDtAcc,
							 cl_inn      => bank.get_inn_acc(iDtAcc),
							 cl_name     => bank.Get_Account_Name(iDtAcc,
																  'N'),
							 co_mfo      => vMfo,
							 co_acc      => iKrAcc,
							 co_inn      => bank.get_inn_acc(iKrAcc),
							 co_name     => bank.Get_Account_Name(iKrAcc,
																  'N'),
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
			IF Err_Msg IS NOT NULL
			THEN
				INSERT INTO FZ_DEP_ERR
				VALUES
					(vMfo,
					 iManba,
					 substr(Get_Error_Msg_Short(err_Msg), 1, 240),
					 SYSDATE,
					 vOperDay,
					 iSubManba);
			END IF;
		
		EXCEPTION
			WHEN OTHERS THEN
				err_Msg := substr(SQLERRM, 1, 240);
				INSERT INTO FZ_DEP_ERR
				VALUES
					(vMfo, iManba, err_Msg, SYSDATE, vOperDay, iSubManba);
			
				setup.Set_Employee_Code(vCodeOld);
		END;
	END;

	PROCEDURE uchet_vnebal
	(
		iManba     NUMBER
	   ,iSubManba  NUMBER
	   ,iPlusMinus VARCHAR2
	   ,iSumma     NUMBER
	) IS
		vVnebal   Qqb_Hl_Accounts_Vnebal%ROWTYPE;
		vPlusOst  NUMBER;
		vMinusOst NUMBER;
		------------------------------
		vPlusDtAcc  accounts.code%TYPE;
		vPlusKrAcc  accounts.code%TYPE;
		vMinusDtAcc accounts.code%TYPE;
		vMinusKrAcc accounts.code%TYPE;
		vPlusSumma  NUMBER := 0;
		vMinusSumma NUMBER := 0;
	
	BEGIN
	
		SELECT *
		  INTO vVnebal
		  FROM Qqb_Hl_Accounts_Vnebal a
		 WHERE a.manba_id = iManba;
		SELECT bank.Get_Saldo_Out(vVnebal.Acc_Plus_Deb)
		  INTO vPlusOst
		  FROM dual;
		SELECT bank.Get_Saldo_Out(vVnebal.Acc_Minus_Deb)
		  INTO vMinusOst
		  FROM dual;
	
		IF iPlusMinus = 'PLUS'
		THEN
			vPlusDtAcc  := vVnebal.Acc_Plus_Deb;
			vPlusKrAcc  := vVnebal.Acc_Plus_Krd;
			vMinusDtAcc := vVnebal.Acc_Minus_Krd;
			vMinusKrAcc := vVnebal.Acc_Minus_Deb;
			IF vMinusOst <> 0
			THEN
				IF vMinusOst >= iSumma
				THEN
					vPlusSumma  := 0;
					vMinusSumma := iSumma;
				ELSE
					vPlusSumma  := iSumma - vMinusOst;
					vMinusSumma := vMinusOst;
				END IF;
			ELSE
				vPlusSumma  := iSumma;
				vMinusSumma := 0;
			END IF;
		END IF;
		IF iPlusMinus = 'MINUS'
		THEN
			vPlusDtAcc  := vVnebal.Acc_Plus_Krd;
			vPlusKrAcc  := vVnebal.Acc_Plus_Deb;
			vMinusDtAcc := vVnebal.Acc_Minus_Deb;
			vMinusKrAcc := vVnebal.Acc_Minus_Krd;
			IF vPlusOst <> 0
			THEN
				IF vPlusOst >= iSumma
				THEN
					vPlusSumma  := iSumma;
					vMinusSumma := 0;
				ELSE
					vPlusSumma  := vPlusOst;
					vMinusSumma := iSumma - vPlusOst;
				END IF;
			ELSE
				vPlusSumma  := 0;
				vMinusSumma := iSumma;
			END IF;
		END IF;
	
		IF vPlusSumma <> 0
		THEN
			vnebal_lead(iManba,
						iSubManba,
						vPlusDtAcc,
						vPlusKrAcc,
						vPlusSumma,
						'Test');
		END IF;
		IF vMinusSumma <> 0
		THEN
			vnebal_lead(iManba,
						iSubManba,
						vMinusDtAcc,
						vMinusKrAcc,
						vMinusSumma,
						'Test');
		END IF;
	
	
	END;


END qqb_resurs_val;
/
