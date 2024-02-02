module Arkham.Event.Cards.GhastlyRevelation (
  ghastlyRevelation,
  GhastlyRevelation (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Projection

newtype GhastlyRevelation = GhastlyRevelation EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

ghastlyRevelation :: EventCard GhastlyRevelation
ghastlyRevelation = event GhastlyRevelation Cards.ghastlyRevelation

instance RunMessage GhastlyRevelation where
  runMessage msg e@(GhastlyRevelation attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      pushAll
        [ InvestigatorDiscoverCluesAtTheirLocation iid (toSource attrs) 3 Nothing
        , ResolveEvent iid eid Nothing []
        ]
      pure e
    ResolveEvent iid eid _ _ | eid == toId attrs -> do
      clues <- field InvestigatorClues iid
      otherInvestigators <- selectList $ affectsOthers $ notInvestigator iid
      locations <- selectList Anywhere
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [ Label
            "Give any number of your clues to another investigator"
            [ chooseOrRunOne
                player
                [ targetLabel
                  iid'
                  [ chooseAmounts
                      player
                      "Clues to give"
                      (MaxAmountTarget clues)
                      [("Clues", (0, clues))]
                      ( ProxyTarget
                          (toTarget attrs)
                          (InvestigatorTarget iid')
                      )
                  ]
                | iid' <- otherInvestigators
                ]
            ]
          | notNull otherInvestigators
          ]
        <> [ Label
              "Place any number of your clues on any location"
              [ chooseOrRunOne
                  player
                  [ targetLabel
                    lid
                    [ chooseAmounts
                        player
                        "Clues to give"
                        (MaxAmountTarget clues)
                        [("Clues", (0, clues))]
                        (ProxyTarget (toTarget attrs) (LocationTarget lid))
                    ]
                  | lid <- locations
                  ]
              ]
           ]
      pure e
    ResolveAmounts iid (getChoiceAmount "Clues" -> n) (ProxyTarget (isTarget attrs -> True) target) ->
      do
        pushAll
          [ InvestigatorSpendClues iid n
          , PlaceClues (toSource attrs) target n
          , SufferTrauma iid 0 1
          , InvestigatorDefeated (toSource attrs) iid
          ]
        pure e
    _ -> GhastlyRevelation <$> runMessage msg attrs
