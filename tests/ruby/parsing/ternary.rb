    def after(command = nil, &block)
      @after ||= block ? block : command
    end
