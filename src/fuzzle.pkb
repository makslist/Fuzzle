create or replace package body fuzzle is

   wrong_number_or_types_of_arguments exception;
   pragma exception_init(wrong_number_or_types_of_arguments, -306);
   internal_error_code exception;
   pragma exception_init(internal_error_code, -600);
   from_keyword_not_found_where_expected exception;
   pragma exception_init(from_keyword_not_found_where_expected, -923);
   sql_command_not_properly_ended exception;
   pragma exception_init(sql_command_not_properly_ended, -933);
   quoted_string_not_properly_terminated exception;
   pragma exception_init(quoted_string_not_properly_terminated, -1756);
   function_returned_without_value exception;
   pragma exception_init(function_returned_without_value, -6503);

   nl constant varchar2(1) := chr(10);

   subtype str is varchar2(32767);
   type str_list is table of str;
   type num_list is table of number;

   type error_t is record(
       procedure_name str
      ,overload       number
      ,arg_string     str
      ,arg_num        number
      ,error_class    str
      ,sql_code       number
      ,sql_errm       str
      ,error_stack    str
      ,call_stack     str);
   type errors_tab_t is table of error_t;
   errors errors_tab_t := errors_tab_t();

   type max_string_map is table of varchar(32767) index by varchar(30);
   defined_args max_string_map;

   debugging_mode boolean := false;

   cursor procs_with_args(p_owner        in sys.all_arguments.owner%type
                         ,p_package_name in sys.all_arguments.package_name%type
                         ,p_object_name  in sys.all_arguments.object_name%type) is
      select distinct a.owner
                     ,a.package_name
                     ,a.object_name
                     ,a.overload
        from sys.all_arguments a
       where a.owner = upper(p_owner)
         and ((p_package_name is null and upper(p_package_name) != $$plsql_unit) or a.package_name = upper(p_package_name))
         and (p_object_name is null or a.object_name = upper(p_object_name))
         and a.argument_name is not null
         and not exists (select 1
                from sys.all_arguments y
               where a.owner = y.owner
                 and a.package_name = y.package_name
                 and a.object_name = y.object_name
                 and nvl(a.overload, '###') = nvl(y.overload, '###')
                 and a.data_level = y.data_level
                 and y.data_type not in ('VARCHAR2'
                                        ,'RAW'
                                        ,'NCHAR'
                                        ,'BINARY_INTEGER'
                                        ,'BINARY_FLOAT'
                                        ,'CHAR'
                                        ,'NVARCHAR2'
                                        ,'NUMBER'
                                        ,'FLOAT'
                                        ,'LONG RAW')
                 and rownum = 1);

   cursor arguments(p_owner        in sys.all_arguments.owner%type
                   ,p_object_name  in sys.all_arguments.object_name%type
                   ,p_package_name in sys.all_arguments.package_name%type
                   ,p_overload     in sys.all_arguments.overload%type) is
      select a.owner
            ,a.package_name
            ,a.object_name
            ,a.position
            ,a.argument_name
            ,a.data_type
            ,replace(a.in_out, '/', ' ') as in_out
        from sys.all_arguments a
       where a.owner = upper(p_owner)
         and a.object_name = upper(p_object_name)
         and nvl(a.overload, '###') = nvl(p_overload, '###')
         and (p_package_name is null or instr(upper(a.object_name), upper(p_package_name)) > 0 or
             instr(upper(a.package_name), upper(p_package_name)) > 0)
         and a.data_type in ('VARCHAR2'
                            ,'RAW'
                            ,'NCHAR'
                            ,'BINARY_INTEGER'
                            ,'BINARY_FLOAT'
                            ,'CHAR'
                            ,'NVARCHAR2'
                            ,'NUMBER'
                            ,'FLOAT'
                            ,'LONG RAW')
       order by a.owner
               ,a.package_name
               ,a.object_name
               ,a.position;

   fuzzy_char   str_list := str_list('TEST'
                                          ,'SYS'
                                          ,'XMLREF'
                                          ,q'[" || XMLREF() || "]'
                                          ,q'[', 'TEST" A A ]'
                                          ,q'[']'
                                          ,q'["]'
                                          ,rpad('A', 30, 'A')
                                          ,rpad('A', 100, 'A')
                                          ,rpad('A', 128, 'A')
                                          ,rpad('A', 256, 'A')
                                          ,rpad('A', 512, 'A')
                                          ,rpad('A', 1024, 'A')
                                          ,rpad('A', 2048, 'A')
                                          ,rpad('A', 3000, 'A')
                                          ,rpad('A', 4000, 'A')
                                          ,rpad('A', 4096, 'A')
                                          ,rpad('A', 4097, 'A')
                                          ,rpad('A', 5000, 'A')
                                          ,rpad('A', 6000, 'A')
                                          ,rpad('A', 7000, 'A')
                                          ,rpad('A', 8000, 'A')
                                          ,rpad('A', 10000, 'A')
                                          ,rpad('A', 15000, 'A')
                                          ,rpad('A', 20000, 'A')
                                          ,rpad('A', 25000, 'A')
                                          ,rpad('A', 30000, 'A')
                                          ,rpad('A', 32767, 'A')
                                          ,'ROWID'
                                          ,'PRIMARY KEY'
                                          ,'%s%s%s%s%s%s%s'
                                          ,'%x%x%x%x%x%x'
                                          ,'%d%d%d%d%d%d'
                                          ,'GRANT DBA TO TEST'
                                          ,'GRANT DBA TO PUBLIC'
                                          ,'SELECT * FROM DBA_USERS'
                                          ,q'[' OR '1' = '1]'
                                          ,q'[ OR 1 = 1 --]'
                                          ,q'['AA' or ""TEST"]'
                                          ,q'["XMLREF"" ]'
                                          ,q'[V1]'
                                          ,'TEST.V1'
                                          ,q'["TEST"."V1"]'
                                          ,q'[1@2@3]'
                                          ,null);
   fuzzy_number num_list := num_list(-1, -2, 0, 1, 2, 2147483647, -2147483647, 2147483648, -2147483648, null);

   function get_proc_name(p_owner        in varchar2
                         ,p_package_name in varchar2
                         ,p_object_name  in varchar2) return varchar2 is
   begin
      if p_package_name is not null
      then
         return p_owner || '.' || p_package_name || '.' || p_object_name;
      else
         return p_owner || '.' || p_object_name;
      end if;
   end;

   procedure fuzzy_data(p_stmt           in varchar2
                       ,p_string         in varchar2
                       ,p_number         in number
                       ,p_procedure_name in varchar2 default null) is
   
      procedure log_exception(p_procedure_name in varchar2
                              ,p_overload       in number
                              ,p_arg_string     in varchar2
                              ,p_arg_num        in number
                              ,p_error_class    in varchar2) is
      begin
         errors.extend(1);
         errors(errors.last) := error_t(p_procedure_name
                                                 ,p_overload
                                                 ,p_arg_string
                                                 ,p_arg_num
                                                 ,p_error_class
                                                 ,sqlcode
                                                 ,sqlerrm
                                                 ,dbms_utility.format_error_stack
                                                 ,dbms_utility.format_call_stack);
         dbms_output.put_line('*** ' || p_error_class || ' *** ' || p_procedure_name || ' input: ' ||
                              rpad(nvl(p_string, 'null'), 8) || ' length(' || length(p_string) || ') ' || '/' ||
                              p_arg_num || ' code: ' || sqlcode || ' errm: ' || sqlerrm);
         rollback;
      end;
   
   begin
      execute immediate p_stmt
         using p_string, p_number;
      rollback;
   exception
      when value_error then
         rollback;
      when sql_command_not_properly_ended then
         log_exception(p_procedure_name, null, p_string, p_number, 'POSSIBLE SQL INJECTION FOUND');
      when quoted_string_not_properly_terminated then
         log_exception(p_procedure_name, null, p_string, p_number, 'POSSIBLE SQL INJECTION FOUND');
      when from_keyword_not_found_where_expected then
         log_exception(p_procedure_name, null, p_string, p_number, 'POSSIBLE SQL INJECTION FOUND');
      when internal_error_code then
         log_exception(p_procedure_name, null, p_string, p_number, 'INTERNAL ERROR');
      when wrong_number_or_types_of_arguments then
         log_exception(p_procedure_name, null, p_string, p_number, 'Currently unfuzzable');
      when function_returned_without_value then
         log_exception(p_procedure_name, null, p_string, p_number, 'PL/SQL PROGRAMMING ERROR');
    --   when pkg_error.glo_struk_error then
    --      log_exception(p_procedure_name, null, p_string, p_number, 'Not handled yet');
    --   when pkg_error.glo_db_error then
    --      log_exception(p_procedure_name, null, p_string, p_number, 'Not handled yet');
    --   when pkg_error.exc_helas then
    --      rollback;
    --   when pkg_error.pk_null then
    --      rollback;
    --   when pkg_error.exc_oracle then
    --      log_exception(p_procedure_name, null, p_string, p_number, 'MASKED EXCEPTION');
      when others then
         log_exception(p_procedure_name, null, p_string, p_number, 'UNKNOWN ERROR');
   end;

   procedure run(p_owner        in varchar2 default user
                 ,p_package_name in varchar2 default null
                 ,p_object_name  in varchar2 default null) is
      total_procs number := 0;
      block_stmt constant str := q'^declare
   string varchar2(32767) := :string;
   int number := :int;
   l_varchar2 varchar(32767) := string;
   l_number   number := int;
   l_clob     clob;
begin
   execute immediate ^';
      proc_name str := '';
   
      function ternary(condition boolean
                      ,val_1     varchar2
                      ,val_2     varchar2) return varchar2 is
      begin
         return case when condition then val_1 else val_2 end;
      end;
   begin
      for proc in procs_with_args(upper(p_owner), upper(p_package_name), upper(p_object_name))
      loop
         total_procs := total_procs + 1;
         declare
            is_function boolean := false;
            stmt        str;
            call_stmt   str;
            using_stmt  str := nl || '      using ';
         begin
            proc_name := lower(get_proc_name(proc.owner, proc.package_name, proc.object_name));
         
            for arg in arguments(proc.owner, proc.object_name, proc.package_name, proc.overload)
            loop
               if arg.position = 0
               then
                  is_function := true;
                  call_stmt  := call_stmt || ':' || arg.position || ' := ';
                  using_stmt := using_stmt || lower(arg.in_out) || ' l_' || lower(arg.data_type);
               else
                  if arg.position = 1
                  then
                     call_stmt := call_stmt || proc_name || '(';
                  end if;
                  call_stmt  := call_stmt || ternary(arg.position >= 2, ', ', '') || lower(arg.argument_name) || '=>:' ||
                                arg.position;
                  using_stmt := using_stmt || ternary(is_function or arg.position >= 2, ', ', '') || lower(arg.in_out) || ' ' || case
                                   when defined_args.exists(upper(arg.argument_name)) then
                                    '''' || defined_args(arg.argument_name) || ''''
                                   else
                                    'l_' || case
                                       when (arg.in_out = 'OUT' and arg.data_type = 'VARCHAR2') then
                                        'clob'
                                       else
                                        lower(arg.data_type)
                                    end
                                end;
               end if;
            end loop;
            call_stmt := call_stmt || ');';
         
            stmt := block_stmt || '''begin' || nl || '   ' || call_stmt || nl || 'end;''';
            stmt := stmt || using_stmt || ';' || nl || 'end;';
         
            dbms_output.put_line('----------');
            dbms_output.put_line(proc_name);
            if debugging_mode
            then
               dbms_output.put_line(stmt);
            end if;
            for idx in fuzzy_char.first .. fuzzy_char.last
            loop
               fuzzy_data(stmt, fuzzy_char(idx), 0, proc_name);
            end loop;
            for idx in fuzzy_number.first .. fuzzy_number.last
            loop
               fuzzy_data(stmt, 'A', fuzzy_number(idx), proc_name);
            end loop;
         end;
      end loop;
      dbms_output.put_line(nl || 'Fuzzed ' || total_procs || ' procedure(s) and function(s).');
      dbms_output.put_line('Done.');
   exception
      when others then
         dbms_output.put_line('Error ' || sqlcode || ': ' || sqlerrm);
         dbms_output.put_line('While fuzzing index ' || total_procs || ' relative to ' || proc_name);
         raise;
   end;

   procedure add_custom_arg(p_arg_name in varchar2
                       ,p_value    in varchar2) is
   begin
      defined_args(upper(p_arg_name)) := p_value;
   end;

   procedure ignore_exception(p_sqlcode in number) is
   begin
      null;
   end;

   procedure catch_exception(p_sqlcode in number, p_errmsg in varchar2) is
   begin
      null;
   end;

   procedure debug is
   begin
      debugging_mode := true;
   end;

   procedure no_debug is
   begin
      debugging_mode := false;
   end;

end;
/
