module Arkham.Location.Cards.PorteDeLAvancee (
  porteDeLAvancee,
  PorteDeLAvancee (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher

newtype PorteDeLAvancee = PorteDeLAvancee LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

porteDeLAvancee :: LocationCard PorteDeLAvancee
porteDeLAvancee =
  location PorteDeLAvancee Cards.porteDeLAvancee 3 (PerPlayer 1)

instance HasAbilities PorteDeLAvancee where
  getAbilities (PorteDeLAvancee a) =
    withBaseAbilities
      a
      [ restrictedAbility a 1 (Here <> AgendaExists AgendaWithAnyDoom)
          $ ActionAbility []
          $ ActionCost 2
      ]

instance RunMessage PorteDeLAvancee where
  runMessage msg l@(PorteDeLAvancee attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      agendas <- selectList AgendaWithAnyDoom
      agendasWithOtherAgendas <- forToSnd agendas (selectJust . NotAgenda . AgendaWithId)
      player <- getPlayer iid
      push
        $ chooseOrRunOne
          player
          [ targetLabel
            target
            [ RemoveDoom (toAbilitySource attrs 1) (AgendaTarget target) 1
            , PlaceDoom (toAbilitySource attrs 1) (AgendaTarget otherTarget) 1
            , PlaceClues (toAbilitySource attrs 1) (toTarget attrs) 2
            ]
          | (target, otherTarget) <- agendasWithOtherAgendas
          ]
      pure l
    _ -> PorteDeLAvancee <$> runMessage msg attrs
