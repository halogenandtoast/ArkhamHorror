module Arkham.Location.Cards.WitchHauntedWoodsAbandonedMine (
  witchHauntedWoodsAbandonedMine,
  WitchHauntedWoodsAbandonedMine (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection

newtype WitchHauntedWoodsAbandonedMine = WitchHauntedWoodsAbandonedMine LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntedWoodsAbandonedMine :: LocationCard WitchHauntedWoodsAbandonedMine
witchHauntedWoodsAbandonedMine =
  location
    WitchHauntedWoodsAbandonedMine
    Cards.witchHauntedWoodsAbandonedMine
    2
    (PerPlayer 1)

instance HasModifiersFor WitchHauntedWoodsAbandonedMine where
  getModifiersFor (InvestigatorTarget iid) (WitchHauntedWoodsAbandonedMine a) =
    do
      resources <- field InvestigatorResources iid
      pure
        $ toModifiers
          a
          [ CannotInvestigateLocation (toId a)
          | resources >= 3 && resources <= 10
          ]
  getModifiersFor _ _ = pure []

instance HasAbilities WitchHauntedWoodsAbandonedMine where
  getAbilities (WitchHauntedWoodsAbandonedMine attrs) =
    withBaseAbilities
      attrs
      [ limitedAbility (GroupLimit PerRound 1)
          $ restrictedAbility
            attrs
            1
            ( Here
                <> ( InvestigatorExists
                      $ InvestigatorWithAnyResources
                      <> AnyInvestigator
                        [ You
                        , InvestigatorAt
                            (NotLocation $ LocationWithInvestigator You)
                        ]
                   )
            )
          $ FastAbility Free
      ]

-- Note: we above ProxyTarget here by doubling it to include the two and from,
-- it would be nice to have a better way to handle this
instance RunMessage WitchHauntedWoodsAbandonedMine where
  runMessage msg l@(WitchHauntedWoodsAbandonedMine attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      resources <- field InvestigatorResources iid
      let
        checkResources =
          if resources > 0 then id else (<> InvestigatorWithAnyResources)
      iids <-
        selectWithField InvestigatorResources
          $ checkResources
          $ InvestigatorAt
            (NotLocation $ locationWithInvestigator iid)

      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ targetLabel
            iid'
            [ chooseOrRunOne player
                $ [ Label
                    "Move to their pool"
                    [ chooseAmounts
                        player
                        "Choose amount of resources to move"
                        (MaxAmountTarget 3)
                        [("Resources", (0, resources))]
                        ( ProxyTarget
                            (toTarget attrs)
                            ( ProxyTarget
                                (InvestigatorTarget iid)
                                (InvestigatorTarget iid')
                            )
                        )
                    ]
                  | resources > 0
                  ]
                <> [ Label
                    "Move to your pool"
                    [ chooseAmounts
                        player
                        "Choose amount of resources to move"
                        (MaxAmountTarget 3)
                        [("Resources", (0, otherResources))]
                        ( ProxyTarget
                            (toTarget attrs)
                            ( ProxyTarget
                                (InvestigatorTarget iid')
                                (InvestigatorTarget iid)
                            )
                        )
                    ]
                   | otherResources > 0
                   ]
            ]
          | (iid', otherResources) <- iids
          ]

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
