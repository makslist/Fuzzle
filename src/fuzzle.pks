create or replace package fuzzle is

   procedure run(p_owner        in varchar2 default user
                 ,p_package_name in varchar2 default null
                 ,p_object_name  in varchar2 default null);

   procedure debug;

   procedure no_debug;

   procedure add_custom_arg(p_arg_name in varchar2
                       ,p_value    in varchar2);

end;
/