module Arkham.Types.Treachery.Cards.HuntedByByakhee
  ( huntedByByakhee
  , HuntedByByakhee(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype HuntedByByakhee = HuntedByByakhee TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntedByByakhee :: TreacheryCard HuntedByByakhee
huntedByByakhee = treachery HuntedByByakhee Cards.huntedByByakhee

instance TreacheryRunner env => RunMessage env HuntedByByakhee where
  runMessage msg t@(HuntedByByakhee attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (RevelationSkillTest iid source SkillAgility 6)
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source -> do
        t <$ pushAll
          [ DiscardTopOfEncounterDeck iid n (Just $ toTarget attrs)
          , ShuffleIntoEncounterDeck []
          ]
    DiscardedTopOfEncounterDeck iid cards target | isTarget attrs target -> do
      let
        isByakhee = member Byakhee . cdCardTraits . toCardDef
        isOmen = member Omen . cdCardTraits . toCardDef
        byakhee = filter isByakhee cards
        omens = filter isOmen cards
        byakheeMsgs = if null byakhee
          then []
          else
            [ FocusCards $ map EncounterCard cards
            , chooseOne
              iid
              [ TargetLabel
                  (CardIdTarget $ toCardId enemy)
                  [ RemoveFromEncounterDiscard enemy
                  , InvestigatorDrewEncounterCard iid enemy
                  ]
              | enemy <- byakhee
              ]
            , UnfocusCards
            ]
      pushAll
        $ byakheeMsgs
        <> [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1
           | not (null omens)
           ]
      pure t
    _ -> HuntedByByakhee <$> runMessage msg attrs
