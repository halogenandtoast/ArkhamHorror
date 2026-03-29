#!/usr/bin/env python3
"""
更新 cards_zh.json 的简体中文数据
从 /home/tk/arkhamdb-json-data/translations/zh-cn/ 读取翻译文件
"""

import json
import sys
from pathlib import Path

# 配置
CARDS_ZH_PATH = Path("/home/tk/ArkhamHorror/frontend/public/cards_zh.json")
TRANSLATION_DIR = Path("/home/tk/arkhamdb-json-data/translations/zh-cn")
ENCOUNTER_FILE = TRANSLATION_DIR / "encounter.json"
PACK_DIR = TRANSLATION_DIR / "pack"

# 需要更新的字段
UPDATE_FIELDS = [
    "name",
    "subname", 
    "text",
    "flavor",
    "traits",
    "back_text",
    "back_flavor",
    "pack_name",
    "type_name",
    "faction_name",
]


def load_json_file(filepath: Path) -> dict | list:
    """从本地文件加载 JSON 数据"""
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            return json.load(f)
    except FileNotFoundError:
        return {}
    except Exception as e:
        print(f"  错误: 无法读取 {filepath}: {e}")
        return {}


def load_local_cards() -> list[dict]:
    """加载本地 cards_zh.json"""
    print(f"加载本地文件: {CARDS_ZH_PATH}")
    with open(CARDS_ZH_PATH, 'r', encoding='utf-8') as f:
        return json.load(f)


def save_local_cards(cards: list[dict]):
    """保存更新后的 cards_zh.json"""
    print(f"保存更新后的文件: {CARDS_ZH_PATH}")
    # 备份原文件
    backup_path = CARDS_ZH_PATH.with_suffix('.json.backup')
    if not backup_path.exists():
        CARDS_ZH_PATH.rename(backup_path)
        print(f"  已备份原文件到: {backup_path}")
    
    with open(CARDS_ZH_PATH, 'w', encoding='utf-8') as f:
        json.dump(cards, f, ensure_ascii=False, indent=2)
    print("  保存完成")


def find_all_pack_files() -> list[Path]:
    """递归查找所有 pack JSON 文件"""
    json_files = []
    if PACK_DIR.exists():
        for json_file in PACK_DIR.rglob("*.json"):
            json_files.append(json_file)
    return sorted(json_files)


def load_translation_data() -> dict[str, dict]:
    """加载所有简体中文翻译数据，以 code 为键"""
    translations = {}
    
    # 查找所有 pack JSON 文件
    pack_files = find_all_pack_files()
    print(f"\n读取 pack 翻译数据...")
    print(f"  发现 {len(pack_files)} 个 JSON 文件")
    
    success_count = 0
    for i, pack_file in enumerate(pack_files):
        # 显示进度
        rel_path = pack_file.relative_to(PACK_DIR)
        print(f"  [{i+1}/{len(pack_files)}] {rel_path}", end='', flush=True)
        
        data = load_json_file(pack_file)
        
        if data and isinstance(data, list):
            card_count = 0
            for card in data:
                if isinstance(card, dict) and 'code' in card:
                    code = card['code']
                    # 存储翻译数据
                    translations[code] = {
                        k: v for k, v in card.items() 
                        if k in UPDATE_FIELDS and v
                    }
                    card_count += 1
            print(f" -> {card_count} 张")
            success_count += 1
        else:
            print(" -> 跳过")
    
    # 读取 encounter 数据
    print("\n读取 encounter 数据...")
    encounter_data = load_json_file(ENCOUNTER_FILE)
    if encounter_data and isinstance(encounter_data, list):
        for card in encounter_data:
            if isinstance(card, dict) and 'code' in card:
                code = card['code']
                translations[code] = {
                    k: v for k, v in card.items() 
                    if k in UPDATE_FIELDS and v
                }
        print(f"  encounter: {len(encounter_data)} 张卡牌")
    
    print(f"\n成功读取 {success_count}/{len(pack_files)} 个文件")
    print(f"总共 {len(translations)} 条翻译记录")
    return translations


def update_cards(cards: list[dict], translations: dict[str, dict]) -> tuple[int, int, int]:
    """更新卡牌数据，返回 (更新数, 匹配数, 总数)"""
    updated = 0
    matched = 0
    total = len(cards)
    
    for card in cards:
        code = card.get('code', '')
        if not code:
            continue
            
        # 查找对应的翻译
        trans = translations.get(code)
        if not trans:
            continue
        
        matched += 1
        
        # 更新字段
        has_update = False
        for field in UPDATE_FIELDS:
            if field in trans and trans[field]:
                # 只在有变化时更新
                if card.get(field) != trans[field]:
                    card[field] = trans[field]
                    has_update = True
        
        if has_update:
            updated += 1
    
    return updated, matched, total


def main():
    print("=" * 60)
    print("Arkham Horror Cards_zh.json 更新工具")
    print("=" * 60)
    print()
    
    # 检查本地文件是否存在
    if not CARDS_ZH_PATH.exists():
        print(f"错误: 文件不存在 {CARDS_ZH_PATH}")
        sys.exit(1)
    
    # 检查翻译目录是否存在
    if not TRANSLATION_DIR.exists():
        print(f"错误: 翻译目录不存在 {TRANSLATION_DIR}")
        print("请先克隆仓库: git clone https://github.com/Kamalisk/arkhamdb-json-data.git")
        sys.exit(1)
    
    # 加载本地翻译数据
    print("步骤 1: 加载本地翻译数据")
    print("-" * 40)
    translations = load_translation_data()
    
    if not translations:
        print("错误: 未能加载任何翻译数据")
        sys.exit(1)
    
    print()
    
    # 加载本地卡牌数据
    print("步骤 2: 加载本地卡牌数据")
    print("-" * 40)
    cards = load_local_cards()
    print(f"  本地卡牌数量: {len(cards)}")
    print()
    
    # 更新卡牌
    print("步骤 3: 更新卡牌数据")
    print("-" * 40)
    updated, matched, total = update_cards(cards, translations)
    print(f"  匹配卡牌: {matched}/{total}")
    print(f"  更新字段: {updated}/{matched}")
    print()
    
    # 保存
    if updated > 0:
        print("步骤 4: 保存更新后的文件")
        print("-" * 40)
        save_local_cards(cards)
        print()
    else:
        print("步骤 4: 无需更新，跳过保存")
        print()
    
    print("=" * 60)
    print("处理完成!")
    print(f"  匹配卡牌: {matched}/{total}")
    print(f"  更新字段: {updated}")
    print("=" * 60)


if __name__ == "__main__":
    main()
