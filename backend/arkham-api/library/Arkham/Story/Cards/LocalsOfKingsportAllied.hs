module Arkham.Story.Cards.LocalsOfKingsportAllied (localsOfKingsportAllied) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Location
import Arkham.Helpers.Window
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Projection
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted hiding (EnemyDefeated)
import Arkham.Token

newtype LocalsOfKingsportAllied = LocalsOfKingsportAllied StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

localsOfKingsportAllied :: StoryCard LocalsOfKingsportAllied
localsOfKingsportAllied = persistStory $ story LocalsOfKingsportAllied Cards.localsOfKingsportAllied

instance HasAbilities LocalsOfKingsportAllied where
  getAbilities (LocalsOfKingsportAllied a) =
    [ mkAbility a 1 $ forced $ EnemyDefeated #when Anyone ByAny (enemyIs Enemies.declanPearce)
    , restricted a 2 (exists $ assetIs Assets.jewelOfSarnath <> AssetAt YourLocation) actionAbility
    , restricted
        a
        3
        ( exists
            ( assetIs Assets.jewelOfSarnath
                <> AssetControlledBy Anyone
                <> not_ (AssetWithTokens AnyValue Damage)
                <> not_ (AssetWithTokens AnyValue Doom)
            )
        )
        $ Objective
        $ triggered (RoundEnds #when) (GroupClueCost (PerPlayer 3) Anywhere)
    ]

instance RunMessage LocalsOfKingsportAllied where
  runMessage msg s@(LocalsOfKingsportAllied attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (defeatedEnemy -> enemy) _ -> do
      withLocationOf enemy \loc -> do
        mAid <- selectOne $ assetIs Assets.jewelOfSarnath <> AssetAttachedTo (TargetIs $ toTarget enemy)
        for_ mAid \aid -> place aid $ AttachedToLocation loc
      pure s
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      aid <- selectJust $ assetIs Assets.jewelOfSarnath <> assetAtLocationWith iid
      sid <- getRandom
      chooseOneM iid do
        for_ [#combat, #agility] \kind -> do
          skillLabeled kind $ beginSkillTest sid iid (attrs.ability 2) aid kind (Fixed 3)
      pure s
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      aid <- selectJust $ assetIs Assets.jewelOfSarnath <> assetAtLocationWith iid
      tokens <- field AssetTokens aid
      when (countTokens Damage tokens > 0) do
        removeTokens (attrs.ability 2) aid #damage 1
      when (countTokens Doom tokens > 0 && countTokens Damage tokens == 0) do
        removeDoom (attrs.ability 2) aid 1
      chooseOneM iid $ withI18n do
        nameVar Assets.jewelOfSarnath $ labeled' "takeControlOf" $ takeControlOfAsset iid aid
        labeled' "skip" nothing
      pure s
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advanceCurrentAct (attrs.ability 3)
      pure s
    _ -> LocalsOfKingsportAllied <$> liftRunMessage msg attrs
