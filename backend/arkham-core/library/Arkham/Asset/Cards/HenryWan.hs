module Arkham.Asset.Cards.HenryWan (
  henryWan,
  HenryWan (..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Matcher
import Arkham.Prelude
import Arkham.RequestedChaosTokenStrategy

newtype Metadata = Metadata {revealedChaosTokens :: [ChaosToken]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, NoThunks, NFData)

newtype HenryWan = HenryWan (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

henryWan :: AssetCard HenryWan
henryWan = ally (HenryWan . (`with` Metadata [])) Cards.henryWan (1, 2)

instance HasAbilities HenryWan where
  getAbilities (HenryWan (a `With` _)) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility []
        $ ActionCost 1
        <> exhaust a
    ]

instance RunMessage HenryWan where
  runMessage msg a@(HenryWan (attrs `With` meta)) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ RequestChaosTokens (toAbilitySource attrs 1) (Just iid) (Reveal 1) SetAside
      pure a
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      player <- getPlayer iid
      if any ((`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]) . chaosTokenFace) tokens
        then
          push
            $ chooseOne
              player
              [Label "Do nothing" [HandleTargetChoice iid (toAbilitySource attrs 1) (toTarget attrs)]]
        else
          push
            $ chooseOne
              player
              [ Label "Stop" [HandleTargetChoice iid (toAbilitySource attrs 1) (toTarget attrs)]
              , Label "Draw Another" [RequestChaosTokens (toAbilitySource attrs 1) (Just iid) (Reveal 1) SetAside]
              ]
      pure $ HenryWan (attrs `with` Metadata (tokens <> revealedChaosTokens meta))
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) _ -> do
      push $ ResetChaosTokens (toAbilitySource attrs 1)
      unless
        ( any
            ((`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]) . chaosTokenFace)
            (revealedChaosTokens meta)
        )
        $ do
          canDraw <- iid <=~> InvestigatorCanDrawCards Anyone
          canGainResources <- iid <=~> InvestigatorCanGainResources
          when (canDraw || canGainResources) $ do
            player <- getPlayer iid
            msgs <- for (revealedChaosTokens meta) $ \_ -> do
              drawing <- drawCards iid (toAbilitySource attrs 1) 1
              pure
                $ chooseOrRunOne player
                $ [Label "Draw 1 card" [drawing] | canDraw]
                <> [Label "Gain 1 resources" [TakeResources iid 1 (toAbilitySource attrs 1) False] | canGainResources]
            pushAll msgs
      pure $ HenryWan (attrs `with` Metadata [])
    _ -> HenryWan . (`with` meta) <$> runMessage msg attrs
