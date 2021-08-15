module Arkham.Types.Investigator.Cards.ZoeySamaras where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Game.Helpers
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.Window

newtype ZoeySamaras = ZoeySamaras InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

zoeySamaras :: ZoeySamaras
zoeySamaras = ZoeySamaras $ baseAttrs
  "02001"
  "Zoey Samaras"
  Guardian
  Stats
    { health = 9
    , sanity = 6
    , willpower = 4
    , intellect = 2
    , combat = 4
    , agility = 2
    }
  [Believer, Hunter]

instance InvestigatorRunner env => HasAbilities env ZoeySamaras where
  getAbilities iid (AfterEnemyEngageInvestigator who _) (ZoeySamaras InvestigatorAttrs {..})
    | iid == investigatorId && iid == who
    = do
      let
        ability =
          mkAbility (InvestigatorSource investigatorId) 1 (ResponseAbility Free)
      modifiers' <- getModifiers
        (InvestigatorSource investigatorId)
        (InvestigatorTarget investigatorId)
      pure [ ability | CannotGainResources `notElem` modifiers' ]

  getAbilities i window (ZoeySamaras attrs) = getAbilities i window attrs

instance HasTokenValue env ZoeySamaras where
  getTokenValue (ZoeySamaras attrs) iid ElderSign
    | iid == investigatorId attrs = pure
    $ TokenValue ElderSign (PositiveModifier 1)
  getTokenValue (ZoeySamaras attrs) iid token = getTokenValue attrs iid token

instance InvestigatorRunner env => RunMessage env ZoeySamaras where
  runMessage msg i@(ZoeySamaras attrs@InvestigatorAttrs {..}) = case msg of
    UseCardAbility _ (InvestigatorSource iid) _ 1 _ | iid == investigatorId ->
      i <$ push (TakeResources investigatorId 1 False)
    ResolveToken _drawnToken ElderSign iid | iid == investigatorId -> i <$ push
      (CreateWindowModifierEffect
        EffectSkillTestWindow
        (EffectModifiers $ toModifiers attrs [DamageDealt 1])
        (InvestigatorSource investigatorId)
        (InvestigatorTarget investigatorId)
      )
    _ -> ZoeySamaras <$> runMessage msg attrs
