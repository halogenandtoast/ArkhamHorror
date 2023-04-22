module Arkham.Location.Cards.MoldyHallsEarlierTonight (
  moldyHallsEarlierTonight,
  MoldyHallsEarlierTonight (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Investigator.Types (Field (InvestigatorDiscard, InvestigatorName))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Modifier
import Arkham.Name
import Arkham.Projection
import Arkham.ScenarioLogKey

newtype MoldyHallsEarlierTonight = MoldyHallsEarlierTonight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moldyHallsEarlierTonight :: LocationCard MoldyHallsEarlierTonight
moldyHallsEarlierTonight =
  location MoldyHallsEarlierTonight Cards.moldyHallsEarlierTonight 2 (Static 0)

instance HasAbilities MoldyHallsEarlierTonight where
  getAbilities (MoldyHallsEarlierTonight a) =
    withRevealedAbilities
      a
      [ restrictedAbility
          a
          1
          ( Here
              <> InvestigatorExists
                ( DiscardWith AnyCards
                    <> InvestigatorWithoutModifier CardsCannotLeaveYourDiscardPile
                    <> InvestigatorAt (LocationWithId $ toId a)
                )
          )
          $ ActionAbility Nothing
          $ ActionCost 1
      ]

instance RunMessage MoldyHallsEarlierTonight where
  runMessage msg l@(MoldyHallsEarlierTonight attrs) = case msg of
    Msg.RevealLocation _ lid | lid == toId attrs -> do
      MoldyHallsEarlierTonight <$> runMessage msg (attrs & labelL .~ "moldyHallsEarlierTonight")
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      iids <-
        selectWithField InvestigatorDiscard $
          investigatorAt (toId attrs)
            <> DiscardWith AnyCards
            <> InvestigatorWithoutModifier CardsCannotLeaveYourDiscardPile

      iidsWithName <- for iids $ \(iid, discards) -> do
        name <- field InvestigatorName iid
        pure (iid, discards, name)

      pushAll
        [ chooseOne
          iid
          [ Label "Do not request aid from your past self" []
          , Label
              "Request aid from your past self"
              [ FocusCards discards
              , chooseOne
                  iid
                  [targetLabel (toCardId card) [ReturnToHand iid (CardTarget card)] | card <- discards]
              , UnfocusCards
              , Remember $ MeddledWithThePast $ labeled name iid
              ]
          ]
        | (iid, map toCard -> discards, name) <- iidsWithName
        ]
      pure l
    _ -> MoldyHallsEarlierTonight <$> runMessage msg attrs
