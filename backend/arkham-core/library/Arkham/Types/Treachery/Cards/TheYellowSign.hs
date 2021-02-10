module Arkham.Types.Treachery.Cards.TheYellowSign where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.TreacheryId


import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype TheYellowSign = TheYellowSign TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theYellowSign :: TreacheryId -> a -> TheYellowSign
theYellowSign uuid _ = TheYellowSign $ baseAttrs uuid "01176"

instance HasModifiersFor env TheYellowSign where
  getModifiersFor = noModifiersFor

instance HasActions env TheYellowSign where
  getActions i window (TheYellowSign attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env TheYellowSign where
  runMessage msg t@(TheYellowSign attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> t <$ unshiftMessages
      [ BeginSkillTest
        iid
        source
        (InvestigatorTarget iid)
        Nothing
        SkillWillpower
        4
      , Discard $ toTarget attrs
      ]
    FailedSkillTest iid _ source _ _ _ | isSource attrs source ->
      t <$ unshiftMessages
        [ InvestigatorAssignDamage
          iid
          (TreacherySource treacheryId)
          DamageAny
          0
          2
        , SearchDeckForTraits iid (InvestigatorTarget iid) [Madness] -- TODO: We may need to specify weakness
        ]
    _ -> TheYellowSign <$> runMessage msg attrs
