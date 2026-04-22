function setup(config)
  config.action(
    "copy-change-id",
    function()
      local id = context.change_id()
      if not id or id == "" then
        flash({
          text = "No change ID available.",
          error = true,
        })
        return
      end
      copy_to_clipboard(id)
      flash(string.format("Copied change ID \"%s\".", id))
    end)

  for _, scope in ipairs({
    "revisions",
    "revisions.evolog",
  }) do
    config.bind({
      action = "copy-change-id",
      seq = { "Y", "i" },
      scope = scope,
      desc = "copy change's ID",
    })
  end

  config.action(
    "copy-commit-id",
    function()
      local id = context.commit_id()
      if not id or id == "" then
        flash({
          text = "No commit ID available.",
          error = true,
        })
        return
      end
      copy_to_clipboard(id)
      flash(string.format("Copied commit ID \"%s\".", id))
    end)

  for _, scope in ipairs({
    "revisions",
    "revisions.evolog",
  }) do
    config.bind({
      action = "copy-commit-id",
      seq = { "Y", "I" },
      scope = scope,
      desc = "copy commit's ID",
    })
  end

  config.action(
    "copy-file-path",
    function()
      local file_path = context.file()
      if not file_path or file_path == "" then
        flash({
          text = "No file path available.",
          error = true,
        })
        return
      end
      copy_to_clipboard(file_path)
      flash(string.format("Copied file path \"%s\".", file_path))
    end
  )

  config.bind({
    action = "copy-file-path",
    seq = { "Y", "f" },
    scope = "revisions.details",
    desc = "copy file's path",
  })

  config.action(
    "copy-commit-description",
    function()
      local target_type = "commit"
      local id = context.commit_id()
      if not id or id == "" then
        id = context.change_id()
        target_type = "change"
      end
      if id and #id > 0 then
        local description, err = jj(
          "show",
          "--no-patch",
          "--template", "description",
          id)
        if err then
          flash({
            text = err,
            error = true,
          })
          return
        end
        if description:match("^%s*$") then
          flash("No description set.")
          return
        end
        copy_to_clipboard(description)
        flash(string.format("Copied description for %s \"%s\".", target_type, id))
      end
    end)

  for _, scope in ipairs({
    "revisions",
    "revisions.evolog",
  }) do
    config.bind({
      action = "copy-commit-description",
      seq = { "Y", "d" },
      scope = scope,
      desc = "copy commit's description",
    })
  end

  config.action(
    "report-commit-headline-length",
    function()
      local target_type = "Commit"
      local id = context.commit_id()
      if not id or id == "" then
        id = context.change_id()
        target_type = "Change"
      end
      if id and #id > 0 then
        local headline, err = jj(
          "show",
          "--no-patch",
          "--template", "description.first_line()",
          id)
        if err then
          flash({
            text = err,
            error = true,
          })
          return
        end
        local qualifier = ""
        local suffix = headline:match("^WIP: (.*)$")
        if suffix then
          headline = suffix
          qualifier = " (WIP prefix excluded)"
        end
        local headline_length = #headline
        flash({
          text = string.format("%s headline length%s: %d", target_type, qualifier, headline_length),
          error = headline_length > 50,
        })
      end
    end)

  for _, scope in ipairs({
    "revisions",
    "revisions.evolog",
  }) do
    config.bind({
      action = "report-commit-headline-length",
      seq = { "Y", "l" },
      scope = scope,
      desc = "report commit headline's length (bytes)",
    })
  end
end
