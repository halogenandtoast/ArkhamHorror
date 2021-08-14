module Arkham.Types.Location.Cards.Graveyard where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (graveyard)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target

newtype Graveyard = Graveyard LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

graveyard :: LocationCard Graveyard
graveyard =
  location Graveyard Cards.graveyard 1 (PerPlayer 2) Hourglass [Circle]

instance HasModifiersFor env Graveyard

instance ActionRunner env => HasAbilities env Graveyard where
  getAbilities i window (Graveyard attrs) = getAbilities i window attrs

instance (LocationRunner env) => RunMessage env Graveyard where
  runMessage msg l@(Graveyard attrs@LocationAttrs {..}) = case msg of
    AfterEnterLocation iid lid | lid == locationId -> do
      push
        (BeginSkillTest
          iid
          (LocationSource lid)
          (InvestigatorTarget iid)
          Nothing
          SkillWillpower
          3
        )
      Graveyard <$> runMessage msg attrs
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        rivertownId <- getJustLocationIdByName "Rivertown"
        l <$ push
          (chooseOne
            iid
            [ InvestigatorAssignDamage iid source DamageAny 0 2
            , TargetLabel
              (LocationTarget rivertownId)
              [MoveTo iid rivertownId, MovedBy iid (toSource attrs)]
            ]
          )
    _ -> Graveyard <$> runMessage msg attrs
