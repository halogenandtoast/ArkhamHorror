module Arkham.Treachery.Cards.TheYellowSign where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Source
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TheYellowSign = TheYellowSign TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theYellowSign :: TreacheryCard TheYellowSign
theYellowSign = treachery TheYellowSign Cards.theYellowSign

instance RunMessage TheYellowSign where
  runMessage msg t@(TheYellowSign attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source
      | isSource attrs source ->
          t
            <$ push
              ( beginSkillTest
                  iid
                  source
                  (InvestigatorTarget iid)
                  SkillWillpower
                  4
              )
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source ->
          t
            <$ pushAll
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
                  [fromDeck]
                  (CardWithTrait Madness) -- TODO: We may need to specify weakness, candidate for card matcher
                  (DrawFound iid 1)
              ]
    _ -> TheYellowSign <$> runMessage msg attrs
