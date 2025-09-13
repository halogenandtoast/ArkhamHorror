module Arkham.Asset.Assets.BestowResolve2 (bestowResolve2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier

newtype BestowResolve2 = BestowResolve2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bestowResolve2 :: AssetCard BestowResolve2
bestowResolve2 = asset BestowResolve2 Cards.bestowResolve2

instance HasAbilities BestowResolve2 where
  getAbilities (BestowResolve2 a) =
    [ controlled
        a
        1
        ( DuringSkillTest
            ( IfSkillTestMatcher PerilousSkillTest (SkillTestOfInvestigator You)
                $ SkillTestOfInvestigator (affectsOthers $ at_ $ oneOf [YourLocation, connectedFrom YourLocation])
            )
            <> exists
              ( PassesCommitRestrictions
                  $ InHandOf NotForPlay You
                  <> basic (NonWeakness <> oneOf [CardWithAnySkills, SkillCard <> CardWithNoSkills])
              )
            <> youExist (not_ $ HasMatchingAsset $ assetIs Cards.theKingInYellow)
        )
        $ FastAbility (assetUseCost a Charge 1)
    ]

instance RunMessage BestowResolve2 where
  runMessage msg a@(BestowResolve2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectWithNonNull
        ( PassesCommitRestrictions
            $ inHandOf NotForPlay iid
            <> basic (NonWeakness <> oneOf [CardWithAnySkills, SkillCard <> CardWithNoSkills])
        )
        $ chooseOneToHandle iid attrs
      pure a
    HandleTargetChoice iid (isSource attrs -> True) (CardIdTarget cid) -> do
      card <- getCard cid
      withSkillTest \sid ->
        skillTestModifier sid (attrs.ability 1) cid ReplaceAllSkillIconsWithWild
      push $ SkillTestCommitCard iid card
      pure a
    _ -> BestowResolve2 <$> liftRunMessage msg attrs
