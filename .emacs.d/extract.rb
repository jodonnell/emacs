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
      return var.push(thing) if next_is_var
      if thing.is_a? Array
        get_assigned_vars thing, var, assignment
      elsif thing == :assign
        assignment = true
      elsif thing == :@ident and assignment
        next_is_var = true
      end
    end
    return var
  end
end
ast_refactor = ASTRefactor.new
