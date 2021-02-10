module Arkham.Types.Treachery.Cards.ClawsOfSteam
  ( clawsOfSteam
  , ClawsOfSteam(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.TreacheryId


import Arkham.Types.Game.Helpers
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype ClawsOfSteam = ClawsOfSteam TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clawsOfSteam :: TreacheryId -> a -> ClawsOfSteam
clawsOfSteam uuid _ = ClawsOfSteam $ baseAttrs uuid "02180"

instance HasModifiersFor env ClawsOfSteam where
  getModifiersFor = noModifiersFor

instance HasActions env ClawsOfSteam where
  getActions i window (ClawsOfSteam attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env ClawsOfSteam where
  runMessage msg t@(ClawsOfSteam attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> t <$ unshiftMessages
      [ RevelationSkillTest iid source SkillWillpower 3
      , Discard (toTarget attrs)
      ]
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> t <$ unshiftMessages
        [ CreateWindowModifierEffect
          EffectRoundWindow
          (EffectModifiers $ toModifiers attrs [CannotMove])
          source
          (InvestigatorTarget iid)
        , InvestigatorAssignDamage
          iid
          (TreacherySource treacheryId)
          DamageAssetsFirst
          2
          0
        ]
    _ -> ClawsOfSteam <$> runMessage msg attrs
