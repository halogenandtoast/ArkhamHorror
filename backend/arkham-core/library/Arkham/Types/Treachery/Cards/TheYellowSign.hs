module Arkham.Types.Treachery.Cards.TheYellowSign where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.Zone

newtype TheYellowSign = TheYellowSign TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theYellowSign :: TreacheryCard TheYellowSign
theYellowSign = treachery TheYellowSign Cards.theYellowSign

instance TreacheryRunner env => RunMessage env TheYellowSign where
  runMessage msg t@(TheYellowSign attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> t <$ push
      (BeginSkillTest
        iid
        source
        (InvestigatorTarget iid)
        Nothing
        SkillWillpower
        4
      )
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> t <$ pushAll
        [ InvestigatorAssignDamage
          iid
          (TreacherySource treacheryId)
          DamageAny
          0
          2
        , Search
          iid
          source
          (InvestigatorTarget iid)
          FromDeck
          [Madness]
          (ShuffleBackIn $ DrawFound iid)-- TODO: We may need to specify weakness
        ]
    _ -> TheYellowSign <$> runMessage msg attrs
