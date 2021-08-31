module Arkham.Types.Location.Cards.Graveyard where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (graveyard)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype Graveyard = Graveyard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

graveyard :: LocationCard Graveyard
graveyard =
  location Graveyard Cards.graveyard 1 (PerPlayer 2) Hourglass [Circle]

instance HasAbilities env Graveyard where
  getAbilities i window (Graveyard x) = withBaseAbilities i window x $ pure
    [ mkAbility x 1
        $ ForcedAbility (Enters Timing.After Anyone $ LocationWithId (toId x))
    ]

instance LocationRunner env => RunMessage env Graveyard where
  runMessage msg l@(Graveyard attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ push
      (BeginSkillTest
        iid
        source
        (InvestigatorTarget iid)
        Nothing
        SkillWillpower
        3
      )
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        rivertownId <- getJustLocationIdByName "Rivertown"
        l <$ push
          (chooseOne
            iid
            [ Label
              "Take 2 horror"
              [InvestigatorAssignDamage iid source DamageAny 0 2]
            , Label
              "Move to Rivertown"
              [MoveTo (toSource attrs) iid rivertownId]
            ]
          )
    _ -> Graveyard <$> runMessage msg attrs
