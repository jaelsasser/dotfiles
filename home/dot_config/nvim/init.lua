-- ~/.config/nvim/init.lua — sources the plugin-free vim spine, then layers the
-- Lua tools. evil-era habits map to nvim-native mechanisms. Neovim 0.12+.

vim.loader.enable()

-- 1. Shared spine. Its readline block is `!has('nvim')`-gated, so under nvim the
--    insert layer is vim-rsi + §7.
local spine = (vim.env.XDG_CONFIG_HOME or vim.fn.expand('~/.config')) .. '/vim/vimrc'
if vim.fn.filereadable(spine) == 1 then
  vim.cmd.source(spine)
end

vim.o.signcolumn = 'yes'
vim.diagnostic.config({ virtual_text = true })

-- 2. Plugins (vim.pack). treesitter on 'master' auto-installs w/ a bundled
--    compiler; 'main' would need the tree-sitter CLI a container lacks.
vim.pack.add({
  { src = 'https://github.com/nvim-treesitter/nvim-treesitter', version = 'master' },
  'https://github.com/neovim/nvim-lspconfig',
  'https://github.com/folke/flash.nvim',
  'https://github.com/nvim-mini/mini.nvim',
  'https://github.com/tpope/vim-rsi',          -- §7
})

-- Colorscheme discipline: in a terminal, nvim inherits the terminal's ANSI
-- palette via cterm colors — no termguicolors, no truecolor scheme. Only the
-- Neovide GUI themes itself: the colorscheme reads &background, and the autocmd
-- re-applies it so a `:set background=light` flips light/dark live.
if vim.g.neovide then
  vim.pack.add({ 'https://github.com/kepano/flexoki-neovim' })
  vim.o.termguicolors = true
  vim.o.background = 'dark'
  vim.cmd.colorscheme('flexoki')
  vim.api.nvim_create_autocmd('OptionSet', {
    pattern = 'background',
    callback = function() vim.cmd.colorscheme('flexoki') end,
  })
end

-- 3. Tree-sitter. Install parsers only when a compiler exists, else degrade quietly.
local has_cc = vim.fn.executable('cc') == 1
  or vim.fn.executable('gcc') == 1
  or vim.fn.executable('clang') == 1
require('nvim-treesitter.configs').setup({
  ensure_installed = has_cc and {
    'bash', 'c', 'cpp', 'go', 'json', 'lua', 'markdown', 'markdown_inline',
    'python', 'query', 'rust', 'toml', 'vim', 'vimdoc', 'yaml',
  } or {},
  auto_install = false,
  highlight = { enable = true },
  indent    = { enable = true },
})

-- 4. LSP (native client). Minimal core; binaries are env-provided.
vim.lsp.enable({ 'clangd', 'basedpyright', 'lua_ls' })
vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('user_lsp', { clear = true }),
  callback = function(ev)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, { buffer = ev.buf })  -- the gap in 0.11 defaults
    local client = vim.lsp.get_client_by_id(ev.data.client_id)
    if client and client:supports_method('textDocument/completion') then
      vim.lsp.completion.enable(true, client.id, ev.buf, { autotrigger = false })
    end
  end,
})

-- 5. Completion: on-demand, not auto-pop — matches Emacs company (idle-delay nil).
--    <C-Space> asks; insert typing stays quiet.
vim.o.autocomplete = false
vim.o.completeopt  = 'menu,menuone,noselect,popup,fuzzy'
vim.keymap.set('i', '<C-Space>', function() vim.lsp.completion.get() end)

-- 6. Motions / textobjects. flash.jump on `s` unifies evil-snipe (bare `s`) and
--    evil-easymotion (was SPC). `s`/`S` shadow substitute-char/line (use cl/cc).
--    Keymaps stay eager (lazy-require); setups ride vim.schedule off the first frame.
vim.keymap.set({ 'n', 'x', 'o' }, 's', function() require('flash').jump() end)
vim.keymap.set('n',               'S', function() require('flash').treesitter() end)
vim.keymap.set('x', 'S', [[:<C-u>lua MiniSurround.add('visual')<CR>]], { silent = true })

vim.schedule(function()
  require('flash').setup()

  -- evil-surround: ys/ds/cs + visual S.
  require('mini.surround').setup({
    mappings = {
      add = 'ys', delete = 'ds', replace = 'cs',
      find = '', find_left = '', highlight = '', update_n_lines = '',
      suffix_last = '', suffix_next = '',
    },
  })

  -- evil-args (ia/aa) + evil-indent-plus (ii/ai). Indent spec is bespoke (mini.ai
  -- ships none) — validate on Python/YAML before trusting.
  local ai = require('mini.ai')
  ai.setup({
    n_lines = 100,
    custom_textobjects = {
      a = ai.gen_spec.argument({ brackets = { '%b()', '%b[]', '%b{}' }, separator = ',' }),
      i = function(ai_type)
        local cur = vim.fn.line('.')
        local function indent(l) return l > 0 and vim.fn.indent(l) or -1 end
        local base = indent(cur)
        if vim.fn.getline(cur):match('^%s*$') then
          base = math.max(indent(vim.fn.prevnonblank(cur)), indent(vim.fn.nextnonblank(cur)))
        end
        local top, bot = cur, cur
        while top > 1 and (indent(top - 1) >= base or vim.fn.getline(top - 1):match('^%s*$')) do top = top - 1 end
        while bot < vim.fn.line('$') and (indent(bot + 1) >= base or vim.fn.getline(bot + 1):match('^%s*$')) do bot = bot + 1 end
        if ai_type == 'a' then top = math.max(1, top - 1) end
        return { from = { line = top, col = 1 },
                 to   = { line = bot, col = math.max(1, #vim.fn.getline(bot)) } }
      end,
    },
  })
end)

-- 7. Insert readline. vim-rsi covers C-A/B/D/E/F + M-b/M-f/M-d + cmdline; add the
--    Emacs reflexes it misses: C-K kill-to-EOL, C-Y paste, C-G abort (≈ keyboard-quit).
vim.cmd([[
  inoremap <C-K> <C-\><C-O>D
  inoremap <C-Y> <C-R><C-O>"
  inoremap <C-G> <C-\><C-N>
]])
