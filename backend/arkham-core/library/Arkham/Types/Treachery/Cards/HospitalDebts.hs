module Arkham.Types.Treachery.Cards.HospitalDebts
  ( HospitalDebts(..)
  , hospitalDebts
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (InvestigatorEliminated)
import Arkham.Types.Modifier
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype HospitalDebts = HospitalDebts TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

hospitalDebts :: TreacheryCard HospitalDebts
hospitalDebts =
  treachery (HospitalDebts . (resourcesL ?~ 0)) Cards.hospitalDebts

instance HasModifiersFor env HospitalDebts where
  getModifiersFor _ (InvestigatorTarget iid) (HospitalDebts attrs) = do
    let resources' = fromJustNote "must be set" $ treacheryResources attrs
    pure $ toModifiers
      attrs
      [ XPModifier (-2) | treacheryOnInvestigator iid attrs && resources' < 6 ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities HospitalDebts where
  getAbilities (HospitalDebts a) =
    (restrictedAbility
          a
          1
          (OnSameLocation <> InvestigatorExists
            (You <> InvestigatorWithResources (AtLeast $ Static 1))
          )
          (FastAbility Free)
      & abilityLimitL
      .~ PlayerLimit PerRound 2
      )
      : [ restrictedAbility a 2 (ResourcesOnThis $ LessThan $ Static 6)
          $ ForcedAbility
          $ OrWindowMatcher
              [ GameEnds Timing.When
              , InvestigatorEliminated Timing.When (InvestigatorWithId iid)
              ]
        | iid <- maybeToList (treacheryOwner a)
        ]

instance TreacheryRunner env => RunMessage env HospitalDebts where
  runMessage msg t@(HospitalDebts attrs) = case msg of
    Revelation iid source | isSource attrs source -> t <$ pushAll
      [ RemoveCardFromHand iid (toCardId attrs)
      , AttachTreachery (toId attrs) (InvestigatorTarget iid)
      ]
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      t <$ pushAll [SpendResources iid 1, PlaceResources (toTarget attrs) 1]
    -- This is handled as a modifier currently but we still issue the UI ability
    UseCardAbility _ source _ 2 _ | isSource attrs source -> pure t
    _ -> HospitalDebts <$> runMessage msg attrs
