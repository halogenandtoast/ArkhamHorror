Title:
Fix embedded i18n tokens in mixed UI text

Issue ID: TBD
Agnes Baker (Parallel) embedded i18n display bug

Summary:
This fixes frontend rendering for mixed text that contains embedded i18n tokens, such as `Pay $label.doNotTakeDamage`.

Problem:
The frontend i18n handling was internally inconsistent:

- whole-string i18n tokens like `"$label.doNotTakeDamage"` were translated correctly
- mixed strings like `"Pay $label.doNotTakeDamage"` were not translated
- the backend was already emitting the correct i18n key for Agnes Baker (Parallel)
- but the frontend only translated strings when the entire string started with `$`

Because of that, mixed UI text could display raw i18n tokens instead of localized text. In Agnes Baker (Parallel), choosing not to take damage could surface `$LABEL.DONOTTAKEDAMAGE` in the UI after the raw token passed through existing uppercase styling.

Change:

- Added `handleEmbeddedI18n` in `frontend/src/arkham/i18n.ts`
- Preserved existing whole-string `handleI18n` behavior for strings that are entirely an i18n token
- Added embedded-token replacement for bare tokens like `$label.doNotTakeDamage` inside otherwise plain text
- Explicitly left embedded parameterized tokens unsupported in this pass
- Changed log rendering in `frontend/src/arkham/components/GameMessage.vue` to use `handleEmbeddedI18n`
- Changed websocket/local log formatting in `frontend/src/arkham/views/Game.vue` to use `handleEmbeddedI18n`
- Changed question/payment label rendering in `frontend/src/arkham/components/Question.vue` to use `handleEmbeddedI18n`
- Changed modal label rendering in `frontend/src/arkham/components/ChoiceModal.vue` to use `handleEmbeddedI18n`
- Kept raw payment/amount labels in `ChoiceModal.vue` so the existing `.label.` -> `.title.` lookup logic still works
- Changed choice rendering in `frontend/src/arkham/components/QuestionChoices.vue` to use `handleEmbeddedI18n`
- Changed story question rendering in `frontend/src/arkham/components/StoryQuestion.vue` to use `handleEmbeddedI18n`
- Changed dropdown option rendering in `frontend/src/components/DropDown.vue` to use `handleEmbeddedI18n`

Files changed:

- `frontend/src/arkham/i18n.ts`
- `frontend/src/arkham/components/GameMessage.vue`
- `frontend/src/arkham/views/Game.vue`
- `frontend/src/arkham/components/Question.vue`
- `frontend/src/arkham/components/ChoiceModal.vue`
- `frontend/src/arkham/components/QuestionChoices.vue`
- `frontend/src/arkham/components/StoryQuestion.vue`
- `frontend/src/components/DropDown.vue`

Test coverage:
Added regression coverage in the rendering paths for:

- whole-string i18n tokens continuing to render exactly as before
- mixed strings like `Pay $label.doNotTakeDamage` rendering localized text
- multiple embedded bare tokens in the same string being localized
- strings without embedded tokens remaining unchanged
- Agnes Baker (Parallel) mixed-text display paths using the new helper

Validation:
Ran:

`cd frontend && npm run tc`

Result:

- `vue-tsc` still reports many pre-existing TypeScript errors in the frontend codebase
- no new type-check error cluster was introduced by the embedded i18n helper itself
- touched files such as `frontend/src/arkham/i18n.ts`, `frontend/src/arkham/components/GameMessage.vue`, `frontend/src/arkham/components/ChoiceModal.vue`, and `frontend/src/components/DropDown.vue` did not surface new helper-related type errors
