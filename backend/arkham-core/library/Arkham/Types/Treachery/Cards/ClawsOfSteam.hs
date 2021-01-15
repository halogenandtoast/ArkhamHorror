module Arkham.Types.Treachery.Cards.ClawsOfSteam
  ( clawsOfSteam
  , ClawsOfSteam(..)
  ) where

import Arkham.Import

import Arkham.Types.Game.Helpers
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype ClawsOfSteam = ClawsOfSteam Attrs
  deriving newtype (Show, ToJSON, FromJSON)

clawsOfSteam :: TreacheryId -> a -> ClawsOfSteam
clawsOfSteam uuid _ = ClawsOfSteam $ baseAttrs uuid "02180"

instance HasModifiersFor env ClawsOfSteam where
  getModifiersFor = noModifiersFor

instance HasActions env ClawsOfSteam where
  getActions i window (ClawsOfSteam attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env ClawsOfSteam where
  runMessage msg t@(ClawsOfSteam attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> t <$ unshiftMessages
      [ RevelationSkillTest iid source SkillWillpower 3
      , Discard (toTarget attrs)
      ]
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _
      | isSource attrs source -> t <$ unshiftMessages
        [ CreateWindowModifierEffect
          EffectPhaseWindow
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
