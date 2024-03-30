module Arkham.Asset.Cards.BlessedBlade (
  blessedBlade,
  BlessedBlade (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Helpers.ChaosBag
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Message
import Arkham.Prelude

newtype BlessedBlade = BlessedBlade AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blessedBlade :: AssetCard BlessedBlade
blessedBlade =
  assetWith BlessedBlade Cards.blessedBlade (setMeta @Bool True)

instance HasAbilities BlessedBlade where
  getAbilities (BlessedBlade attrs) = [restrictedAbility attrs 1 (ControlsThis <> exists (be attrs <> AssetReady)) fightAction_]

instance RunMessage BlessedBlade where
  runMessage msg a@(BlessedBlade attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll
        [ skillTestModifier (attrs.ability 1) iid (SkillModifier #combat 1)
        , chooseFightEnemy iid (attrs.ability 1) #combat
        ]
      pure a
    RevealChaosToken SkillTestSource iid token -> do
      s <- fromJustNote "Must be in skillTest" <$> getSkillTestSource
      if isAbilitySource attrs 1 s
        then do
          let meta = toResult @Bool attrs.meta
          if meta && token.face `elem` [#bless, #eldersign]
            then do
              push $ skillTestModifier (attrs.ability 1) iid (DamageDealt 1)
              pure . BlessedBlade $ attrs & setMeta @Bool False
            else pure a
        else pure a
    SkillTestEnds _ _ -> do
      pure . BlessedBlade $ attrs & setMeta @Bool True
    BeforeRevealChaosTokens -> do
      void $ runMaybeT $ do
        source <- MaybeT getSkillTestSource
        guard $ isAbilitySource attrs 1 source
        iid <- MaybeT getSkillTestInvestigator
        player <- lift $ getPlayer iid
        canAfford <- lift $ getCanAffordCost iid source [] [] (exhaust attrs)
        hasBlessTokens <- lift $ (> 0) <$> getRemainingBlessTokens
        guard canAfford
        guard hasBlessTokens

        lift
          $ push
          $ chooseOne
            player
            [ Label
                "Exhaust Blessed Blade to add 1 {bless} token"
                [Exhaust (toTarget attrs), AddChaosToken BlessToken]
            , Label "Do not exhaust" []
            ]

      pure a
    _ -> BlessedBlade <$> runMessage msg attrs
