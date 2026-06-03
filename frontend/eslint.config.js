import js from '@eslint/js'
import pluginVue from 'eslint-plugin-vue'
import tseslint from 'typescript-eslint'
import prettierSkipFormatting from '@vue/eslint-config-prettier/skip-formatting'
import vueParser from 'vue-eslint-parser'

export default tseslint.config(
  {
    ignores: [
      'dist/**',
      'node_modules/**',
      'coverage/**',
      'image-manifest.json',
      '.eslintrc.cjs',
      'harness/**',
      'scripts/**',
      'src/types/**/*.d.ts',
    ],
  },

  js.configs.recommended,
  ...tseslint.configs.recommended,
  ...pluginVue.configs['flat/essential'],
  prettierSkipFormatting,

  {
    files: ['**/*.vue'],
    languageOptions: {
      parser: vueParser,
      parserOptions: {
        parser: tseslint.parser,
        ecmaVersion: 'latest',
        sourceType: 'module',
        extraFileExtensions: ['.vue'],
      },
    },
  },

  {
    files: ['**/*.{ts,vue}'],
    languageOptions: {
      ecmaVersion: 'latest',
      sourceType: 'module',
    },
    rules: {
      'vue/multi-word-component-names': 'off',
      'vue/no-v-html': 'off',
      'vue/no-ref-as-operand': 'off',
      'vue/valid-v-for': 'off',
      'vue/require-v-for-key': 'off',
      'vue/no-use-v-if-with-v-for': 'off',
      'vue/return-in-computed-property': 'off',
      'vue/no-mutating-props': 'off',
      'vue/require-toggle-inside-transition': 'off',
      '@typescript-eslint/no-explicit-any': 'off',
      '@typescript-eslint/no-empty-object-type': 'off',
      '@typescript-eslint/no-unused-vars': [
        'warn',
        { argsIgnorePattern: '^_', varsIgnorePattern: '^_' },
      ],
      '@typescript-eslint/ban-ts-comment': 'off',
      'no-unused-vars': 'off',
      'no-undef': 'off',
      'no-useless-escape': 'off',
      'no-cond-assign': 'off',
      'no-empty': 'off',
      'no-fallthrough': 'off',
      'no-case-declarations': 'off',
    },
  },
)
