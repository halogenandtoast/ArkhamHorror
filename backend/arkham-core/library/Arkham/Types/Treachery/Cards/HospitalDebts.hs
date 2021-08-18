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
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner
import Arkham.Types.Window

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

ability :: TreacheryAttrs -> Ability
ability a = (mkAbility (toSource a) 1 (FastAbility Free))
  { abilityLimit = PlayerLimit PerRound 2
  }

instance ActionRunner env => HasAbilities env HospitalDebts where
  getAbilities iid (Window Timing.When (DuringTurn who)) (HospitalDebts a)
    | iid == who = withTreacheryInvestigator a $ \tormented -> do
      resourceCount <- getResourceCount iid
      treacheryLocationId <- getId tormented
      investigatorLocationId <- getId @LocationId iid
      pure
        [ ability a
        | resourceCount > 0 && treacheryLocationId == investigatorLocationId
        ]
  getAbilities _ _ _ = pure []

instance TreacheryRunner env => RunMessage env HospitalDebts where
  runMessage msg t@(HospitalDebts attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> t <$ pushAll
      [ RemoveCardFromHand iid (toCardId attrs)
      , AttachTreachery treacheryId (InvestigatorTarget iid)
      ]
    UseCardAbility iid (TreacherySource tid) _ 1 _ | tid == treacheryId -> do
      push (SpendResources iid 1)
      pure $ HospitalDebts
        (attrs { treacheryResources = (+ 1) <$> treacheryResources })
    _ -> HospitalDebts <$> runMessage msg attrs
