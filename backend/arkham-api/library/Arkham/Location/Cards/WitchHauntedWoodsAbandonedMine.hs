module Arkham.Location.Cards.WitchHauntedWoodsAbandonedMine (
  witchHauntedWoodsAbandonedMine,
  WitchHauntedWoodsAbandonedMine (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection

newtype WitchHauntedWoodsAbandonedMine = WitchHauntedWoodsAbandonedMine LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntedWoodsAbandonedMine :: LocationCard WitchHauntedWoodsAbandonedMine
witchHauntedWoodsAbandonedMine =
  location
    WitchHauntedWoodsAbandonedMine
    Cards.witchHauntedWoodsAbandonedMine
    2
    (PerPlayer 1)

instance HasModifiersFor WitchHauntedWoodsAbandonedMine where
  getModifiersFor (InvestigatorTarget iid) (WitchHauntedWoodsAbandonedMine a) = do
    resources <- field InvestigatorResources iid
    toModifiers a [CannotInvestigateLocation (toId a) | resources >= 3 && resources <= 10]
  getModifiersFor _ _ = pure []

instance HasAbilities WitchHauntedWoodsAbandonedMine where
  getAbilities (WitchHauntedWoodsAbandonedMine a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted
        a
        1
        ( Here
            <> ( exists
                  $ InvestigatorWithAnyResources
                  <> oneOf [You, InvestigatorAt (not_ $ LocationWithInvestigator You)]
               )
        )
      $ FastAbility Free

-- Note: we above ProxyTarget here by doubling it to include the two and from,
-- it would be nice to have a better way to handle this
instance RunMessage WitchHauntedWoodsAbandonedMine where
  runMessage msg l@(WitchHauntedWoodsAbandonedMine attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      resources <- field InvestigatorResources iid
      let checkResources = if resources > 0 then id else (<> InvestigatorWithAnyResources)
      iids <-
        selectWithField InvestigatorResources
          $ checkResources
          $ InvestigatorAt (not_ $ locationWithInvestigator iid)

      player <- getPlayer iid

      choices <- for iids \(iid', otherResources) -> do
        chooseMsg1 <-
          chooseAmounts
            player
            "Choose amount of resources to move"
            (MaxAmountTarget 3)
            [("Resources", (0, resources))]
            $ ProxyTarget (toTarget attrs)
            $ ProxyTarget (InvestigatorTarget iid) (InvestigatorTarget iid')
        chooseMsg2 <-
          chooseAmounts
            player
            "Choose amount of resources to move"
            (MaxAmountTarget 3)
            [("Resources", (0, otherResources))]
            $ ProxyTarget (toTarget attrs)
            $ ProxyTarget (InvestigatorTarget iid') (InvestigatorTarget iid)

        pure
          $ targetLabel
            iid'
            [ chooseOrRunOne player
                $ [Label "Move to their pool" [chooseMsg1] | resources > 0]
                <> [Label "Move to your pool" [chooseMsg2] | otherResources > 0]
            ]

      push $ chooseOne player choices
      pure l
    ResolveAmounts
      _
      (getChoiceAmount "Resources" -> n)
      ( ProxyTarget
          (isTarget attrs -> True)
          (ProxyTarget (InvestigatorTarget fromInvestigator) (InvestigatorTarget toInvestigator))
        ) ->
        do
          pushAll
            [ TakeResources toInvestigator n (toSource attrs) False
            , SpendResources fromInvestigator n
            ]
          pure l
    _ -> WitchHauntedWoodsAbandonedMine <$> runMessage msg attrs
