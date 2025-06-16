module Arkham.Asset.Assets.JewelOfSarnath (
  jewelOfSarnath,
  JewelOfSarnath(..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype JewelOfSarnath = JewelOfSarnath AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jewelOfSarnath :: AssetCard JewelOfSarnath
jewelOfSarnath = storyAsset JewelOfSarnath Cards.jewelOfSarnath

instance HasAbilities JewelOfSarnath where
  getAbilities (JewelOfSarnath a) =
    [ mkAbility a 1 $ forced $ AssetLeavesPlay #when (be a)
    , controlledAbility a 2 (exists $ enemyAtLocationWith You) $
        FastAbility (exhaust a)
    ]

instance RunMessage JewelOfSarnath where
  runMessage msg a@(JewelOfSarnath attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      pushAll
        [ putCardIntoPlay iid attrs
        , PlaceTokens (toSource attrs) (toTarget attrs) Damage 3
        , PlaceDoom (toSource attrs) (toTarget attrs) 1
        ]
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ ShuffleIntoDeck Deck.EncounterDeck (toTarget attrs)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseOne iid
        [ Label "Use your {willpower}" $ beginSkillTest sid iid (attrs.ability 2) iid #willpower (Fixed 1)
        , Label "Use your {agility}" $ beginSkillTest sid iid (attrs.ability 2) iid #agility (Fixed 1)
        ]
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      enemies <- select $ enemyAtLocationWith iid
      let
        damageOpts enemy =
          [ Label "Move 1 damage" [MoveTokens (attrs.ability 2) (toTarget attrs) (toTarget enemy) Damage 1] | attrs.damage > 0 ]
        doomOpts enemy =
          [ Label "Move 1 doom" [MoveTokens (attrs.ability 2) (toTarget attrs) (toTarget enemy) Doom 1] | attrs.doom > 0 ]
      player <- getPlayer iid
      chooseOrRunOne player
        [ targetLabel enemy (damageOpts enemy <> doomOpts enemy)
        | enemy <- enemies
        , notNull (damageOpts enemy <> doomOpts enemy)
        ]
      pure a
    _ -> JewelOfSarnath <$> liftRunMessage msg attrs
