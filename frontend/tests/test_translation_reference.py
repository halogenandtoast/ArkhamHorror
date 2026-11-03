import argparse
import importlib.util
import json
from pathlib import Path
import sys
sys.dont_write_bytecode = True
import tempfile
import unittest

parser = argparse.ArgumentParser(add_help=False)
parser.add_argument('--tool-deps')
options, remaining = parser.parse_known_args()
if options.tool_deps:
    sys.path.insert(0, str(Path(options.tool_deps).resolve()))

import polib

script = Path(__file__).resolve().parents[1] / 'scripts/import-arkham-cards-translations.py'
spec = importlib.util.spec_from_file_location('translation_reference', script)
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)


class ReferenceImportTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.source = self.root / 'reference'

    def catalog(self, locale, entries, filename='messages.po'):
        file = self.source / 'i18n' / locale / 'campaigns/fhv' / filename
        file.parent.mkdir(parents=True, exist_ok=True)
        catalog = polib.POFile()
        catalog.metadata = {'Content-Type': 'text/plain; charset=UTF-8'}
        for english, translated, *flags in entries:
            catalog.append(polib.POEntry(msgid=english, msgstr=translated, flags=flags))
        catalog.save(str(file))

    def reference(self):
        return module.Reference(self.source)

    def test_simplified_translation_wins_when_both_exist(self):
        self.catalog('zh-cn', [('Open the door.', '推开门。')])
        self.catalog('zh', [('Open the door.', '打開大門。')])
        translated, sources = self.reference().translate('Open the door.', ('fhv',))
        self.assertEqual(translated, '推开门。')
        self.assertEqual(sources[0]['locale'], 'zh-cn')

    def test_empty_simplified_translation_uses_traditional_converted_to_simplified(self):
        self.catalog('zh-cn', [('Open the door.', '')])
        self.catalog('zh', [('Open the door.', '打開大門。')])
        translated, sources = self.reference().translate('Open the door.', ('fhv',))
        self.assertEqual(translated, '打开大门。')
        self.assertEqual(sources[0]['locale'], 'zh')
        self.assertTrue(sources[0]['convertedToSimplified'])

    def test_fuzzy_simplified_entry_does_not_override_finished_traditional(self):
        self.catalog('zh-cn', [('Open the door.', '错误草稿。', 'fuzzy')])
        self.catalog('zh', [('Open the door.', '打開大門。')])
        translated, sources = self.reference().translate('Open the door.', ('fhv',))
        self.assertEqual(translated, '打开大门。')
        self.assertEqual(sources[0]['locale'], 'zh')

    def test_runtime_parameters_and_escaped_icons_survive_reference_marker_conversion(self):
        self.catalog('zh-cn', [('[action] Take #count# resources.', '[action] 获得#count#个资源。')])
        original = "{'{'}action{'}'} Take {count} resources."
        translated, _ = self.reference().translate(original)
        self.assertEqual(translated, "{'{'}action{'}'} 获得{count}个资源。")
        self.assertEqual(module.tokens(original), module.tokens(translated))

    def test_reference_that_loses_a_parameter_is_rejected(self):
        self.catalog('zh-cn', [('Take #count# resources.', '获得资源。')])
        self.assertIsNone(self.reference().translate('Take {count} resources.'))

    def test_reference_that_changes_a_numeric_rule_is_rejected(self):
        self.catalog('zh-cn', [('Take 2 resources.', '获得3个资源。')])
        self.assertIsNone(self.reference().translate('Take 2 resources.'))

    def test_conflicting_wordings_require_review(self):
        self.catalog('zh-cn', [('The act.', '当前幕。')])
        self.catalog('zh-cn', [('The act.', '当前行动。')], 'conflict.po')
        self.assertIsNone(self.reference().translate('The act.', ('fhv',)))

    def test_format_only_variants_do_not_create_a_false_conflict(self):
        self.catalog('zh-cn', [('I want a challenge', '<i>来点挑战。</i>')])
        self.catalog('zh-cn', [('Standard <i>(I want a challenge)</i>', '普通<i>(来点挑战。)</i>')], 'difficulty.po')
        translated, _ = self.reference().translate('I want a challenge')
        self.assertEqual(translated, '来点挑战。')

    def test_aligned_paragraphs_preserve_the_original_html_structure(self):
        self.catalog('zh-cn', [('Open the door.\nLeave the house.', '打开门。\n离开房屋。')])
        original = "<p class='basic'>Open the door.</p><p class='right'>Leave the house.</p>"
        translated, _ = self.reference().translate(original)
        self.assertEqual(translated, "<p class='basic'>打开门。</p><p class='right'>离开房屋。</p>")

    def test_incomplete_array_import_does_not_insert_nulls_or_hide_fallback(self):
        original = {'rules': [{'title': 'First rule', 'body': 'Read the rule.'}]}
        proposals = [{'parts': ['rules', 0, 'title'], 'translation': '第一条规则'}]
        kept, rejected = module.preserves_arrays(original, {}, proposals)
        self.assertEqual(kept, [])
        self.assertEqual(rejected, 1)

    def test_dry_run_and_apply_preserve_existing_chinese_and_report_differences(self):
        self.catalog('zh-cn', [('Open the door.', '打开门。'), ('Leave the house.', '离开房屋。')])
        english = self.root / 'frontend/src/locales/en/base.json'
        chinese = self.root / 'frontend/src/locales/zh-cn/base.json'
        english.parent.mkdir(parents=True)
        chinese.parent.mkdir(parents=True)
        english.write_text(json.dumps({'existing': 'Open the door.', 'missing': 'Leave the house.'}), encoding='utf-8')
        chinese.write_text(json.dumps({'existing': '保留我的译文。'}, ensure_ascii=False), encoding='utf-8')
        before = chinese.read_bytes()
        preview = module.run(self.root, self.reference(), apply=False)
        self.assertEqual(chinese.read_bytes(), before)
        self.assertEqual(preview['filledMessages'], 1)
        self.assertEqual(len(preview['existingChineseDifferences']), 1)
        result = module.run(self.root, self.reference(), apply=True)
        updated = json.loads(chinese.read_text(encoding='utf-8'))
        self.assertEqual(updated['existing'], '保留我的译文。')
        self.assertEqual(updated['missing'], '离开房屋。')
        self.assertEqual(result['filledMessages'], 1)


    def test_homebrew_additions_share_the_existing_subtree_owner(self):
        self.catalog('zh-cn', [
            ('Open the door.', '打开门。'),
            ('Leave the house.', '离开房屋。'),
        ])
        english = self.root / 'frontend/homebrew/dark-matter/locales/en/electricNightmare.json'
        chinese = self.root / 'frontend/homebrew/dark-matter/locales/zh-cn/base.json'
        english.parent.mkdir(parents=True)
        chinese.parent.mkdir(parents=True)
        source = {'electricNightmare': {
            'title': 'Open the door.', 'label': {'existing': 'Leave the house.'},
        }}
        existing = {'electricNightmare': {'label': {'existing': '保留已有中文。'}}}
        english.write_text(json.dumps(source), encoding='utf-8')
        chinese.write_text(json.dumps(existing, ensure_ascii=False), encoding='utf-8')
        catalogs = {'en': {'darkMatter': source}, 'zh': {'darkMatter': existing}}
        result = module.run(self.root, self.reference(), apply=True, catalogs=catalogs)
        updated = json.loads(chinese.read_text(encoding='utf-8'))
        self.assertEqual(updated['electricNightmare']['title'], '打开门。')
        self.assertEqual(updated['electricNightmare']['label']['existing'], '保留已有中文。')
        self.assertFalse(chinese.with_name('electricNightmare.json').exists())
        self.assertEqual(result['filledMessages'], 1)
        self.assertEqual(len(result['existingChineseDifferences']), 1)


if __name__ == '__main__':
    unittest.main(argv=[sys.argv[0], *remaining])
