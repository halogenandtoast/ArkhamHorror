<script lang="ts" setup>
import { useI18n } from 'vue-i18n'

const { t } = useI18n()

type MapLocationId = string

interface LocationData {
  travel: number | null
  unlocked: boolean
  subtitle?: string
}

interface MapData {
  current: string
  hasTicket: boolean
  available: MapLocationId[]
}

const props = defineProps<{
  selectedLocation: MapLocationId
  locationData: Record<MapLocationId, LocationData>
  mapData: MapData
  embark: boolean
  isFinale: boolean
}>()

const emit = defineEmits<{
  close: []
  travelTo: []
  travelVia: []
  travelWithTicket: []
}>()
</script>

<template>
  <header>
    <button class="close-btn" @click.stop="emit('close')">×</button>
    <h2>{{ t(`theScarletKeys.locations.${selectedLocation}.name`) }}</h2>
    <h3>{{ locationData[selectedLocation].subtitle }}</h3>
  </header>

  <div class="drawer-content">
    <template v-if="selectedLocation === mapData.current">
      <p v-if="!isFinale">You are currently here.</p>
      <button v-else class="action" @click="emit('travelTo')">Travel here</button>
    </template>
    <template v-else-if="embark">
      <p><strong>Travel time:</strong> {{ locationData[selectedLocation].travel }}</p>
      <div v-if="selectedLocation === 'Venice'" class="side-story-info">
        <p>This is a side-story location.</p>
        <p>If you wish to add a side-story to this campaign, you may travel to this location and spend additional time equal to the normal experience for playing that side story.</p>
      </div>
      <template v-if="locationData[selectedLocation].unlocked">
        <button class="action" @click="emit('travelTo')">Travel here</button>
        <button
          v-if="mapData.hasTicket && (locationData[selectedLocation].travel ?? 0) > 1"
          class="action"
          @click="emit('travelWithTicket')"
        >Travel with Expedited Ticket (1 time)</button>
        <button class="action secondary" @click="emit('travelVia')">Travel here without stopping</button>
      </template>
      <template v-else>
        <p class="action locked">This location is currently locked.</p>
        <button class="action secondary" @click="emit('travelVia')">Travel here without stopping</button>
      </template>
    </template>

    <div v-if="selectedLocation === 'Marrakesh'" class='dossier'>
      <header><h3>You may read this dossier at any time</h3></header>
      <section>
        <p>File #11–B<br/>Subject Class: Red<br/>Real Identity: Unknown<br/>Last Known Location: Marrakesh, Morocco</p>
        <p>Description: Subject #11–B (hereinafter "Amaranth") is a woman of European descent; appears to be in early 20s. Typically seen wearing a large red flower that partly obscures her face.</p>
        <p>Paradimensional Capabilities: Power to <span class='censor'>transfer life</span> and <span class='censor'>death</span>. Channels <span class='censor'>energy</span> via <span class='censor'>extradimensional means</span> to <span class='censor'>heal wounded entities</span> and <span class='censor'>sap life</span> from <span class='censor'>the living</span>. No recorded limits to this <span class='censor'>exchange</span>; data limited only to minor events. Subject <span class='censor'>transfers life</span> via tactile contact, which limits <span class='censor'>scale</span> of <span class='censor'>transfer</span>. Possible <span class='censor'>near apocalyptic</span> ramifications.</p>
        <p>Sightings: 1. November 23, 1923: Arkham, MA. Agents witnessed subject healing wounds of <span class='censor'>fellow Coterie</span> member, <span class='censor'>draining and shriveling</span> nearby trees. Elevated <span class='censor'>Outsider/Paracausal</span> activity reported in the months following.</p>
        <p class='indent'>2. January 11, 1924: Lisbon, Portugal. Local cell apprehended subject. Physical contact confirmed. One cell member immediately deceased; second victim remains catatonic. Subject eluded questioning.</p>
        <p class='indent'>3. Unconfirmed sighting in Marrakesh. Further intel required. Immediate assistance requested.</p>
        <p>Approach Procedure: Subject is incredibly dangerous. Engage only with extreme caution. Physical contact prohibited. More information available in San Francisco office (File #26–G2).</p>
      </section>
    </div>
    <div v-else-if="selectedLocation === 'BuenosAires'" class='dossier'>
      <header><h3>You may read this dossier at any time</h3></header>
      <section>
        <p>File #16–D<br/>Subject Class: Yellow<br/>Real Identity: Unknown<br/>Last Known Location: Buenos Aires, Argentina</p>
        <p>Description: Subject #16–D (hereinafter "Girl in Carmine Coat") is a woman of apparently Argentinian descent (<span class='censor'>possible human</span> origin?), early 20s, approximately 165cm in height. <span class='censor'>Extraterrestrial</span> presence undetected (as of yet). Coterie paraphernalia consists of stark red coat and matching hat possessive of paradimensional faculty (see details below). Called "La Chica Roja" by locals (no doubt a reference to her attire, although it is unclear whether such title is a term of endearment or infamy).</p>
        <p>Paradimensional Capabilities: Reports by local authorities suggest Girl in Carmine Coat is capable of either <span class='censor'>hyperphysical speed</span> or <span class='censor'>ethereal</span> <span class='censor'>projection</span>. Local authorities appear incapable of capturing subject despite repeated attempts. (Can she manipulate <span class='censor'>darkness</span> and <span class='censor'>shadows</span>? Or is she simply that good at evading detection?) Subject seems to wish to avoid conflict; as of yet, no deaths or harm to any locals can be traced to Girl in Carmine Coat. (Cannot rule out potential for violent escalation.)</p>
        <p>Sightings: Girl in Carmine Coat is responsible for several high profile burglaries in Buenos Aires. Reports by local papers indicate she is still at large.</p>
        <p>Approach Procedure: Despite her apparent <span class='censor'>coterie</span> association, Girl in Carmine Coat does not appear to be hostile. She has been heard speaking fluent Spanish, Portugese, French, and English. For these reasons, we believe subject can be interrogated and/or reasoned with. Recommend capture and questioning regarding <span class='censor'>coterie</span> motives.</p>
        <p>Persons of Interest: Oficial Principal Matias Bolívar (ma-tee-as boh-lee-vahr), principal officer in charge of Girl in Carmine Coat's capture. (Possible asset?)</p>
      </section>
    </div>
    <div v-else-if="selectedLocation === 'Constantinople'" class='dossier'>
      <header><h3>You may read this dossier at any time</h3></header>
      <section>
        <p>File #21–F<br/>Subject Class: Green<br/>Real Identity: Şahin, Ece (shah-heen, eh- jay)<br/>Last Known Location: Constantinople, Turkey</p>
        <p>Description: Subject #21–F (hereinafter "Lady in Vermillion Veil" or real name "Ece Şahin") is a woman of Turkish descent, 34 years of age, approximately 161cm in height. It is unknown whether this is subject's <span class='censor'>true form</span> or a disguise or false identity. At all times, subject wears traditional hijab of unusual stark red color, likely a <span class='censor'>paradimensionally bound</span> object designating coterie membership.</p>
        <p>Paradimensional Capabilities: Lady in Vermillion Veil appears to either possess no such capabilities, or has successfully hidden such capabilities from Foundation intelligence.</p>
        <p>Sightings: Ece Şahin is a well known and renowned art collector and museum curator operating in the Turkish and Islamic Arts Museum in Constantinople. She has either <span class='censor'>integrated completely with human society</span> or is, for all intents and purposes, a normal everyday person. (Note: Leadership is under assumption <span class='censor'>Ece Şahin is a well constructed cover story, but has no evidence to back up such assumptions.</span>)</p>
        <p>Approach Procedure: Ece has already reached out to Foundation envoys. Dispatch to confront and question intentions.</p>
      </section>
    </div>
    <div v-else-if="selectedLocation === 'SanFrancisco' || selectedLocation === 'Moscow'" class='dossier'>
      <header><h3>You may read this dossier at any time</h3></header>
      <section>
        <p>File #26–G1–6<br />Sanctum Class: Green<br />Sanctum Locations: <span class='censor'>Cape Town</span>, <span class='censor'>Union of South Africa</span>; San Francisco, California; <span class='censor'>Seoul</span>, <span class='censor'>South Korea</span>; Moscow, Russia; <span class='censor'>Bruges</span>, <span class='censor'>Belgium</span>; <span class='censor'>Bern</span>, <span class='censor'>Switzerland</span>.</p>
        <p>Description: Sanctums #26–G1 through G6 are Foundation offices and storehouses for paradimensional artifacts. Do not disclose locations of sanctums with non–Foundation personnel under penalty of <span class='censor'>expulsion and/or indefinite imprisonment</span>.</p>
        <p>Approach Procedure: Agent in charge of sanctum will meet you upon arrival. Entry instructions attached.</p>
      </section>
    </div>
    <div v-else-if="selectedLocation === 'Havana'" class='dossier'>
      <header><h3>You may read this dossier at any time</h3></header>
      <section>
        <p>File #28–I<br />Subject Class: Yellow<br />Real Identity: Delgado Álvarez, Desiderio (del-gah-doh al-vah-rez, deh-see-deh-ree-oh)<br />Last Known Location: Havana, Cuba</p>
        <p>Description: Subject #28–I (hereinafter "Man in Blood–Soaked Suit" or "Desiderio Delgado Álvarez") is a man of Cuban descent, approximately 42 years old and 192cm in height. Typically seen in a black suit accessorized with apparent coterie paraphernalia, including tie and hat, all with red accents.</p>
        <p>Paradimensional Capabilities: Man in Blood–Soaked Suit does not appear to possess any extraordinary powers aside from peak <span class='censor'>human</span> physical performance, and unusually high skill and accuracy with firearms. (<span class='censor'>Perhaps</span> enhanced <span class='censor'>by a Key?</span>) Approach with caution.</p>
        <p>Sightings: Mr. Álvarez is a longtime resident of Havana. However, no records exist pertaining to subject's childhood or early life. (Likely destroyed upon induction into Coterie, but possibility remains of <span class='censor'>extraterrestrial origin</span>. Either way, his identity may be an alias.) Mr. Álvarez is known to frequent a nightclub known as Cafe Luna.</p>
        <p>Note: Foundation has no knowledge pertaining to any Key in Havana, however, Mr. Álvarez is likely to know location of other Keys.</p>
        <p>Approach Procedure: Open negotiations with subject to acquire location of Coterie vaults and/or hideouts. If he does not cooperate, subdue and interrogate. More information available in Moscow office (File #26–G4).</p>
      </section>
    </div>
    <div v-else-if="selectedLocation === 'Shanghai'" class='dossier'>
      <header><h3>You may read this dossier at any time</h3></header>
      <section>
        <p>File #32–J<br />Asset Name: Flint, Li<br />Area of Operation: Shanghai, China</p>
        <p>Profile: Recently acquired agent in charge of undisclosed cell reporting directly to Commissioner Taylor.</p>
        <p>Current Assignment: According to recent report, asset has split from rest of cell and is currently in Shanghai investigating whereabouts of subject #46–Q.</p>
        <p>Notes: Asset loyalty to Foundation cause is unsure. Extreme vigilance recommended.</p>
      </section>
    </div>
    <div v-else-if="selectedLocation === 'Anchorage'" class='dossier'>
      <header><h3>You may read this dossier at any time</h3></header>
      <section>
        <p>File #33–K<br />Subject Class: Yellow<br />Real Identity: Unknown.<br />Last Known Location: Anchorage, Alaska.</p>
        <p>Description: Subject #33–K (hereinafter "Thorne") is a tall, gaunt person with androgynous features and fair complexion. Prefers practical, loose–fitting clothing. Subject appears to be <span class='censor'>of Anglo–Celtic descent</span> and conjectured to be far older than they appear (collaborating source suggests D.O.B. in <span class='censor'>1761</span>). Subject wears a distinct red cravat around their neck, often obscuring their face.</p>
        <p>Paradimensional Capabilities: Subject possesses acute sensitivity to <span class='censor'>otherworldly residue</span>. Coterie leverages these capabilities to locate <span class='censor'>otherworldly</span> artifacts and track movement of <span class='censor'>paradimensional entities</span>.</p>
        <p>Sightings: Numerous sightings across the globe. Profiled as one of the most mobile Coterie members. Despite notorious secrecy, Thorne has a reputation for appraising and acquiring <span class='censor'>paradimensional</span> artifacts. Analysis suggests they are amenable to business exchange and receptive to haggling. Recent activity suggests Coterie is in a state of <span class='censor'>aggressive acquisition</span>.</p>
        <p>Approach Procedure: You and Thorne likely have similar objectives. Extreme caution suggested. Thorne is likely to negotiate but only on their terms.</p>
      </section>
    </div>
    <div v-else-if="selectedLocation === 'Tokyo'" class='dossier'>
      <header><h3>You may read this dossier at any time</h3></header>
      <section>
        <p>File #37–M<br />Asset Name: Taylor, Qiana<br />Area of Operation: See current assignment status.</p>
        <p>Profile: <span class='censor'>Commissioner Taylor was appointed as head of the Foundation by unanimous consent. Most people will never see the rest of this text, because I really want this whole section to just look like an enormous block of redacted "who knows what she is up to" kind of thing, so I will redact it, but somebody in the PDF might still be able to read it, so oh well. Hey, if you're reading this: kudos, that is awesome. I appreciate that you cared enough to go through the effort of figuring out what is here. Thank you for playing. Much love.</span></p>
        <p>Current Assignment: <span class='censor'>Oh, here we go with the rest of the text, then, okay. Commissioner is looking into reports of paradimensional activity in a number of places. Current task list includes</span> Tokyo, Japan; <span class='censor'>Prague, Czechoslovakia; and</span> Lagos, Nigeria.</p>
      </section>
    </div>
    <div v-else-if="selectedLocation === 'Alexandria'" class='dossier'>
      <header><h3>You may read this dossier at any time</h3></header>
      <section>
        <p>File #38–N<br />Subject Class: Red<br />Real Identity: Unknown<br />Last Known Location: Alexandria, Egypt</p>
        <p>Description: Subject #38–N (hereinafter "Beast in Cowl of Crimson") is a roughly humanoid figure, approximately 183cm in height, clothed in a red cloak. The hood of this cloak effectively masks the figure's identity (very likely to be <span class='censor'>extraterrestrial</span> in origin). Only appendage as of yet observed is a set of <span class='censor'>wolflike</span> claws <span class='censor'>covered in gray fur</span>, hence the moniker. <span class='censor'>Inhuman origin</span> seems likely.</p>
        <p>Paradimensional Capabilities: Beast in Cowl of Crimson appears to have high aptitude for physical confrontation. Prior encounters have led to <span class='censor'>the deaths of several agents</span>. Victims are <span class='censor'>eviscerated</span> and <span class='censor'>left to bleed out</span>, or <span class='censor'>partially devoured and battered around like a bored cat playing with its food</span>. Extreme caution is advised.</p>
        <p>Sightings: Beast in Cowl of Crimson is prime suspect in several gruesome murders throughout Alexandria.</p>
        <p>Note: Paradimensional implement (aka "Key") known to exist in Alexandria. Coterie element may be using key to <span class='censor'>transform or mutate their body</span>.</p>
        <p>Approach Procedure: Subdue subject with extreme prejudice and acquire paradimensional implement for further study.</p>
      </section>
    </div>
    <div v-else-if="selectedLocation === 'RioDeJaneiro' || selectedLocation === 'Perth'" class='dossier'>
      <header><h3>You may read this dossier at any time</h3></header>
      <section>
        <p>File #44–O/55–X<br />Asset Name: Irawan, Dewi<br />Area of Operation: Southern Hemisphere</p>
        <p>Profile: Prominent zoologist has published several surveys regarding disappearing wildlife. Zoologist claims entire species and the memory of their existence are being erased. Possible <span class='censor'>paracausal</span> disturbance.</p>
      </section>
    </div>
    <div v-else-if="selectedLocation === 'Sydney'" class='dossier'>
      <header><h3>You may read this dossier at any time</h3></header>
      <section>
        <p>File #49–R<br />Asset Name: Quinn, Ari<br />Area of Operation: Sydney, Australia</p>
        <p>Profile: Field researcher in charge of paradimensional analysis. Despite recent events, asset has not yet requested transfer. Recommend continued psychiatric evaluation.</p>
        <p>Current Assignment: Agent Quinn is currently performing independent research regarding recent paradimensional disturbances.</p>
      </section>
    </div>
    <div v-else-if="selectedLocation === 'YborCity'" class='dossier'>
      <header><h3>You may read this dossier at any time</h3></header>
      <section>
        <p>File #52–U<br />Sanctum Class: Yellow<br />Sanctum Location: Ybor City, Florida</p>
        <p>Description: Sanctum #52–U is an abandoned cigar factory in Ybor City, north of McKay Bay.</p>
        <p>Sightings: Subject #28–I known to frequent sanctum. Be on alert.</p>
        <p>Approach Procedure: Enter with caution and search for signs of paradimensional implements. Take any and all evidence into Foundation custody.</p>
      </section>
    </div>
    <div v-else-if="selectedLocation === 'Kathmandu'" class='dossier'>
      <header><h3>You may read this dossier at any time</h3></header>
      <section>
        <p>File #53–V<br />Subject Class: Undetermined<br />Real Identity: Uperetria, Aliki Zoni (oo-peh-reh-tree- ah, a-lee-kee zoh-nee)<br />Last Known Location: Kathmandu, Nepal</p>
        <p>Description: Subject #53–V (hereinafter "Maid With Scarlet Sash") appears as a teenage girl of unknown descent wearing a white dress and red sash. <span class='censor'>It is unclear as of now whether such physical form is a true representation or a false image</span>.</p>
        <p>Paradimensional Capabilities: <span class='censor'>Aliki is thought to possess uncanny knowledge beyond her years, and is likely much, much older than she appears. Foundation has yet to confirm paradimensional capabilities of bound key, if any. Subject may be incorporeal</span>.</p>
        <p>Sightings: Locals in region have reported seeing a "spirit" matching subject's description, heralded by a high–pitched whistling sound. Unclear if <span class='censor'>Maid with Scarlet Sash is indeed ectoplasmic in nature, or if her paradimensional abilities reinforce such a description</span>.</p>
        <p>Approach Procedure: Subject Risk Class uncomfirmed. Do not approach.</p>
      </section>
    </div>
    <div v-else-if="selectedLocation === 'Nairobi'" class='dossier'>
      <header><h3>You may read this dossier at any time</h3></header>
      <section>
        <p>File #54–W<br />Subject Class: Green<br />Real Identity: Masai, Tuwile (muh-sah-ee, t-wayl)<br />Last Known Location: Nairobi, Kenya</p>
        <p>Description: Subject #54–W (hereinafter "Tuwile Masai") is a slim man of Kenyan descent, middle–aged, and 176cm in height. Coterie signifiers include large red spectacles, <span class='censor'>access to and disposal</span> of <span class='censor'>Coterie funding</span>, and <span class='censor'>confirmed contact</span> with Coterie operatives. Subject has written and published openly under their name in numerous geological and archeological periodicals.</p>
        <p>Paradimensional Capabilities: No known reports. Subject appears to possess no such capabilities, or has successfully hidden <span class='censor'>several bound paradimensional implements</span> from Foundation intelligence.</p>
        <p>Sightings: Tuwile Masai teaches under a fellowship at Oxford University and has published numerous surveys on work at and around Lake Victoria.</p>
        <p>Approach Procedure: Masai has spurned all Foundation contact thus far. Operatives may wish to approach only if Masai has reason to trust them.</p>
      </section>
    </div>
    <div v-else-if="selectedLocation === 'Kabul'" class='dossier'>
    </div>
  </div>
</template>

<style scoped>
header {
  background: rgba(255,255,255,0.1);
  padding: 0.3em 0.5em;
  border-bottom: 1px solid rgba(255,255,255,0.15);
  position: relative;
  display: flex;
  flex-direction: column;
  justify-content: center;
  min-height: 1.5em;
}

h2 {
  color: white;
  margin: 0 0 0.1em;
}

h3 {
  font-size: 0.6em;
  font-weight: normal;
  color: #ccc;
  margin: 0;
}

.close-btn {
  position: absolute;
  top: 0.25em;
  right: 0.5em;
  background: none;
  border: none;
  color: #ccc;
  cursor: pointer;
  font-size: 1.5em;
  line-height: 1;
}

.drawer-content {
  flex: 1;
  overflow-y: auto;
  padding: 0 1em 1em;
  scrollbar-width: thin;
  margin-top: 0.5em;
}

.action {
  display: block;
  width: 100%;
  margin-top: 0.375em;
  padding: 0.375em;
  border-radius: 4px;
  font-weight: bold;
  background: #2e3a4f;
  color: #eee;
  font-size: 1em;
  cursor: pointer;
  border: none;
  &:hover { background: #3b4a6b; }
}

.action.secondary {
  background: #e2dfcc;
  color: #222;
  &:hover { background: #ccc9b3; }
}

p.locked {
  color: #888;
  background-color: darkred;
  font-style: italic;
}

.side-story-info {
  text-align: left;
  margin-block: 1em;
  display: flex;
  flex-direction: column;
  gap: 0.5em;
}

.dossier {
  margin-top: 1.5em;
  padding: 1em;
  background: #F9F1DE;
  color: #222;
  font-family: 'Typewriter', serif;
  border-radius: 8px;
  font-size: 0.8em;
  line-height: 1.4;
  text-align: left;

  section {
    display: flex;
    flex-direction: column;
    gap: 1em;
  }

  header {
    margin-bottom: 0.5em;
    text-align: center;
    background: none;
    border: none;
    padding: 0;
    min-height: unset;
  }

  h3 {
    align-self: center;
    text-align: center;
    border-bottom: 1px solid #000;
    display: inline-block;
    position: relative;
    color: black;
    font-weight: bold;
    font-size: 1.4em;
    font-family: 'Teutonic', serif;
    &::after {
      content: '';
      position: absolute;
      inset: 0;
      bottom: 3px;
      border-bottom: 1px solid #000;
    }
  }
}

.censor {
  background-color: black;
  color: black;
  padding: 0 4px;
  border-radius: 2px;
}

.indent {
  text-indent: 1.5em;
}
</style>
