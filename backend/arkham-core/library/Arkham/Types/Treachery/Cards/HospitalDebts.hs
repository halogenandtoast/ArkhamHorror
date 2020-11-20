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

instance HasModifiersFor env HospitalDebts where
  getModifiersFor _ (InvestigatorTarget iid) (HospitalDebts attrs) = do
    let resources' = fromJustNote "must be set" $ treacheryResources attrs
    pure
      [ XPModifier (-2)
      | iid `elem` treacheryAttachedInvestigator attrs && resources' < 6
      ]
  getModifiersFor _ _ _ = pure []

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
        usedAbilities <- map unUsedAbility <$> getList ()
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
    Revelation iid source | isSource attrs source -> do
      t <$ unshiftMessages
        [ RemoveCardFromHand iid "01011"
        , AttachTreachery treacheryId (InvestigatorTarget iid)
        ]
    UseCardAbility iid (TreacherySource tid) _ 1 | tid == treacheryId -> do
      unshiftMessage (SpendResources iid 1)
      pure $ HospitalDebts
        (attrs { treacheryResources = (+ 1) <$> treacheryResources })
    _ -> HospitalDebts <$> runMessage msg attrs
