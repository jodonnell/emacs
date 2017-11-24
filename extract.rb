class ASTRefactor
  def find_used_assigned_vars ruby_ast, var, assigned_vars
    next_is_var = false
    ruby_ast.each do |thing|
      return var.push(thing) if next_is_var and assigned_vars.include? thing
      if thing.is_a? Array
        find_used_assigned_vars thing, var, assigned_vars
      elsif thing == :@ident
        next_is_var = true
      end
    end
    return var
  end

  def get_assigned_vars ruby_ast, var, assignment=false
    next_is_var = false
    ruby_ast.each do |thing|
      return var.push(thing).uniq if next_is_var
      if thing.is_a? Array
        get_assigned_vars thing, var, assignment
      elsif thing == :assign
        assignment = true
      elsif thing == :@ident and assignment
        next_is_var = true
        assignment = false
      end
    end
    return var
  end

  def get_local_variables_from_caller ruby_ast, local_variables
    ruby_ast.each_with_index do |thing, index|
      if thing.is_a? Array
        get_local_variables_from_caller thing, local_variables
      elsif thing == :assign
        return local_variables.push(ruby_ast[index + 1][1][1])
      end
    end
    return local_variables
  end

  def get_variable_references ruby_ast, variable_references
    ruby_ast.each_with_index do |thing, index|
      if thing.is_a? Array
        get_variable_references thing, variable_references
        #elsif thing == :var_ref and ruby_ast[index + 1][0] == :@ident
      elsif thing == :vcall and ruby_ast[index + 1][0] == :@ident
        puts ruby_ast[index + 1][1]
        return variable_references.push(ruby_ast[index + 1][1])
      end
    end
    return variable_references
  end

end
