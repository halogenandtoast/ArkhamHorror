module Arkham.Act.Cards.TheMoonsCore (TheMoonsCore (..), theMoonsCore) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype TheMoonsCore = TheMoonsCore ActAttrs
  deriving anyclass (IsAct)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMoonsCore :: ActCard TheMoonsCore
theMoonsCore = act (3, A) TheMoonsCore Cards.theMoonsCore Nothing

instance HasModifiersFor TheMoonsCore where
  getModifiersFor (InvestigatorTarget _) (TheMoonsCore x) = do
    -- Clues cannot be discovered from The Black Core except as a result of a successful investigation.
    pure
      $ toModifiers x [CannotDiscoverCluesExceptAsResultOfInvestigation (locationIs Locations.theBlackCore)]
  getModifiersFor _ _ = pure []

instance HasAbilities TheMoonsCore where
  getAbilities (TheMoonsCore x) =
    [ restrictedAbility
      x
      1
      ( exists (locationIs Locations.theBlackCore <> LocationWithoutClues)
          <> EachUndefeatedInvestigator (investigatorAt Locations.theBlackCore)
      )
      $ Objective
      $ ForcedAbility AnyWindow
    | onSide A x
    ]

instance RunMessage TheMoonsCore where
  runMessage msg a@(TheMoonsCore attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      placeSetAsideLocation_ Locations.cavernsBeneathTheMoonLightSide
      placeSetAsideLocation_ Locations.lightSideOfTheMoon
      placeSetAsideLocation_ Locations.theWhiteShip
      advanceActDeck attrs
      pure a
    _ -> TheMoonsCore <$> liftRunMessage msg attrs
