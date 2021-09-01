module Arkham.Types.Location.Cards.FarAboveYourHouse where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (farAboveYourHouse)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (RevealLocation)
import Arkham.Types.SkillType
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype FarAboveYourHouse = FarAboveYourHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

farAboveYourHouse :: LocationCard FarAboveYourHouse
farAboveYourHouse = location
  FarAboveYourHouse
  Cards.farAboveYourHouse
  2
  (PerPlayer 1)
  Moon
  [Triangle]

instance HasAbilities env FarAboveYourHouse where
  getAbilities i window (FarAboveYourHouse attrs) =
    withBaseAbilities i window attrs $ pure
      [ mkAbility attrs 1
        $ ForcedAbility
        $ RevealLocation Timing.After You
        $ LocationWithId
        $ toId attrs
      | locationRevealed attrs
      ]

instance (LocationRunner env) => RunMessage env FarAboveYourHouse where
  runMessage msg l@(FarAboveYourHouse attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ push
      (BeginSkillTest
        iid
        (toSource attrs)
        (InvestigatorTarget iid)
        Nothing
        SkillWillpower
        4
      )
    FailedSkillTest _ _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source -> do
        investigatorIds <- getInvestigatorIds
        l <$ pushAll
          (concat $ replicate @[[Message]]
            n
            [ RandomDiscard iid' | iid' <- investigatorIds ]
          )
    _ -> FarAboveYourHouse <$> runMessage msg attrs
