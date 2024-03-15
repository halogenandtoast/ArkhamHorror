module Arkham.Treachery.Cards.OminousPortents (
  ominousPortents,
  OminousPortents (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Helpers
import Arkham.Helpers.Modifiers
import Arkham.Keyword (Keyword (Peril))
import Arkham.Scenarios.TheWagesOfSin.Helpers
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype OminousPortents = OminousPortents TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ominousPortents :: TreacheryCard OminousPortents
ominousPortents = treachery OminousPortents Cards.ominousPortents

-- TODO: enemy spawning
-- TODO: surge, I don't think anything cancels surge currently so this is fine
instance RunMessage OminousPortents where
  runMessage msg t@(OminousPortents attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      mTopSpectralCard <- headMay . unDeck <$> getSpectralDeck
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [ Label
            "Draw the top card of the spectral encounter deck. That card gains peril, and its effects cannot be canceled."
            [ cardResolutionModifiers attrs (toCardId topSpectralCard) [AddKeyword Peril, EffectsCannotBeCanceled]
            , InvestigatorDrewEncounterCard iid (topSpectralCard {ecAddedPeril = True})
            ]
          | topSpectralCard <- maybeToList mTopSpectralCard
          ]
        <> [ Label
              "Test {willpower} (3). If you fail take 2 horror."
              [revelationSkillTest iid attrs SkillWillpower 3]
           ]
      pure t
    _ -> OminousPortents <$> runMessage msg attrs
