{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Treachery.Cards.HospitalDebts
  ( HospitalDebts(..)
  , hospitalDebts
  )
where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype HospitalDebts = HospitalDebts Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

hospitalDebts :: TreacheryId -> Maybe InvestigatorId -> HospitalDebts
hospitalDebts uuid iid = HospitalDebts
  $ (weaknessAttrs uuid iid "01011") { treacheryResources = Just 0 }

instance HasModifiersFor env HospitalDebts where
  getModifiersFor _ (InvestigatorTarget iid) (HospitalDebts attrs) = do
    let resources' = fromJustNote "must be set" $ treacheryResources attrs
    pure $ toModifiers
      attrs
      [ XPModifier (-2) | treacheryOnInvestigator iid attrs && resources' < 6 ]
  getModifiersFor _ _ _ = pure []

ability :: Attrs -> Ability
ability a = (mkAbility (toSource a) 1 (FastAbility Free))
  { abilityLimit = PlayerLimit PerRound 2
  }

instance ActionRunner env => HasActions env HospitalDebts where
  getActions iid (DuringTurn You) (HospitalDebts a@Attrs {..}) =
    withTreacheryInvestigator a $ \tormented -> do
      resourceCount <- getResourceCount iid
      treacheryLocationId <- getId tormented
      investigatorLocationId <- getId @LocationId iid
      pure
        [ ActivateCardAbilityAction iid (ability a)
        | resourceCount > 0 && treacheryLocationId == investigatorLocationId
        ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env HospitalDebts where
  runMessage msg t@(HospitalDebts attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> t <$ unshiftMessages
      [ RemoveCardFromHand iid "01011"
      , AttachTreachery treacheryId (InvestigatorTarget iid)
      ]
    UseCardAbility iid (TreacherySource tid) _ 1 | tid == treacheryId -> do
      unshiftMessage (SpendResources iid 1)
      pure $ HospitalDebts
        (attrs { treacheryResources = (+ 1) <$> treacheryResources })
    _ -> HospitalDebts <$> runMessage msg attrs
