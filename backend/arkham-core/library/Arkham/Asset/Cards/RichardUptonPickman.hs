module Arkham.Asset.Cards.RichardUptonPickman (richardUptonPickman, RichardUptonPickman (..)) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Types (Field (EnemyCard))
import Arkham.Helpers.Modifiers hiding (skillTestModifiers)
import Arkham.Helpers.SkillTest
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Source
import Arkham.Target
import Arkham.Trait (Trait (Ghoul))

newtype RichardUptonPickman = RichardUptonPickman AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

richardUptonPickman :: AssetCard RichardUptonPickman
richardUptonPickman = asset RichardUptonPickman Cards.richardUptonPickman

instance HasModifiersFor RichardUptonPickman where
  getModifiersFor (InvestigatorTarget _) (RichardUptonPickman a) = do
    mSource <- getSkillTestSource
    mAction <- getSkillTestAction
    case (mAction, mSource) of
      (Just Action.Fight, Just source) | isSource a source -> do
        let n = count (`cardMatch` IsEncounterCard) $ assetCardsUnderneath a
        pure $ toModifiers a [DamageDealt n | n > 0]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities RichardUptonPickman where
  getAbilities (RichardUptonPickman a) =
    [ controlledAbility a 1 (exists $ EnemyAt YourLocation <> withTrait Ghoul)
        $ FastAbility (exhaust a)
    , restrictedAbility a 2 ControlsThis $ fightAction (exhaust a)
    ]

instance RunMessage RichardUptonPickman where
  runMessage msg a@(RichardUptonPickman attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      ghouls <- selectWithField EnemyCard (enemyAtLocationWith iid <> withTrait Ghoul)
      chooseOne
        iid
        [targetLabel ghoul [PlaceUnderneath (toTarget attrs) [card]] | (ghoul, card) <- ghouls]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      skillTestModifiers (attrs.ability 1) iid [BaseSkillOf #combat 5, NoStandardDamage]
      chooseFightEnemy iid (attrs.ability 1) #combat
      pure a
    _ -> RichardUptonPickman <$> lift (runMessage msg attrs)
