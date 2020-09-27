{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.HospitalDebts where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import Arkham.Types.Window
import ClassyPrelude
import Lens.Micro
import Safe (fromJustNote)

newtype HospitalDebts = HospitalDebts Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

hospitalDebts :: TreacheryId -> Maybe InvestigatorId -> HospitalDebts
hospitalDebts uuid iid = HospitalDebts
  $ (weaknessAttrs uuid iid "01011") { treacheryResources = Just 0 }

instance (ActionRunner env investigator) => HasActions env investigator HospitalDebts where
  getActions i (DuringTurn You) (HospitalDebts Attrs {..}) =
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
        pure
          [ ActivateCardAbilityAction (getId () i) ability
          | resourceCount i
            > 0
            && getId () i
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
    UseCardAbility iid _ (TreacherySource tid) _ 1 | tid == treacheryId -> do
      unshiftMessage (SpendResources iid 1)
      pure $ HospitalDebts
        (attrs { treacheryResources = (+ 1) <$> treacheryResources })
    _ -> HospitalDebts <$> runMessage msg attrs
