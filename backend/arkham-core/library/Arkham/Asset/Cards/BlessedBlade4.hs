module Arkham.Asset.Cards.BlessedBlade4 (blessedBlade4, BlessedBlade4 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Helpers.ChaosBag
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Prelude

newtype BlessedBlade4 = BlessedBlade4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blessedBlade4 :: AssetCard BlessedBlade4
blessedBlade4 = asset BlessedBlade4 Cards.blessedBlade4

instance HasAbilities BlessedBlade4 where
  getAbilities (BlessedBlade4 attrs) = [controlledAbility attrs 1 (exists $ be attrs <> AssetReady) fightAction_]

instance RunMessage BlessedBlade4 where
  runMessage msg a@(BlessedBlade4 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid (attrs.ability 1)
      pushAll
        [ skillTestModifier sid (attrs.ability 1) iid (SkillModifier #combat 2)
        , skillTestModifier sid (attrs.ability 1) (SkillTestTarget sid) ReturnBlessedToChaosBag
        , chooseFight
        ]
      pure a
    BeforeRevealChaosTokens -> do
      void $ runMaybeT $ do
        source <- MaybeT getSkillTestSource
        guard $ isAbilitySource attrs 1 source
        iid <- MaybeT getSkillTestInvestigator
        player <- lift $ getPlayer iid
        canAfford <- lift $ getCanAffordCost iid source [] [] (exhaust attrs)
        blessTokens <- lift $ min 2 <$> getRemainingBlessTokens
        guard canAfford
        guard $ blessTokens > 0

        lift
          $ push
          $ chooseOne
            player
            [ Label ("Exhaust Blessed Blade (4) to add " <> pluralize blessTokens "{bless} token")
                $ Exhaust (toTarget attrs)
                : replicate blessTokens (AddChaosToken #bless)
            , Label "Do not exhaust" []
            ]

      pure a
    _ -> BlessedBlade4 <$> runMessage msg attrs
