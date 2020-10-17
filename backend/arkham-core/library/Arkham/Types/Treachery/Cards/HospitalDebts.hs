{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.HospitalDebts where

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

instance ActionRunner env => HasActions env HospitalDebts where
  getActions iid (DuringTurn You) (HospitalDebts Attrs {..}) =
    case treacheryAttachedInvestigator of
      Nothing -> pure []
      Just attachedInvestigator' -> do
        let
          ability =
            (mkAbility
                (TreacherySource treacheryId)
                1
                (FastAbility (DuringTurn You))
              )
              { abilityLimit = PerRound
              }
        usedAbilities <- map unUsedAbility <$> asks (getList ())
        resourceCount <- getResourceCount iid
        pure
          [ ActivateCardAbilityAction iid ability
          | resourceCount
            > 0
            && iid
            == attachedInvestigator'
            && length
                 (filter (== (attachedInvestigator', ability)) usedAbilities)
            < 2
          ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env HospitalDebts where
  runMessage msg t@(HospitalDebts attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      unshiftMessages
        [ RemoveCardFromHand iid "01011"
        , AttachTreachery tid (InvestigatorTarget iid)
        ]
      HospitalDebts <$> runMessage msg (attrs & attachedInvestigator ?~ iid)
    EndOfGame | fromJustNote "must be set" treacheryResources < 6 ->
      let
        investigator =
          fromJustNote "missing investigator" treacheryAttachedInvestigator
      in
        t <$ unshiftMessage
          (AddModifiers
            (InvestigatorTarget investigator)
            (TreacherySource treacheryId)
            [XPModifier (-2)]
          )
    UseCardAbility iid (TreacherySource tid) _ 1 | tid == treacheryId -> do
      unshiftMessage (SpendResources iid 1)
      pure $ HospitalDebts
        (attrs { treacheryResources = (+ 1) <$> treacheryResources })
    _ -> HospitalDebts <$> runMessage msg attrs
